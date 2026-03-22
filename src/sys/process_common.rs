use std::io::{self, Read, Write};
use std::process::{Child, Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicU8, Ordering};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::arena::{Arena, ArenaString};
use crate::process::{
    OutputPolicy, ProcessCaps, ProcessError, ProcessResult, ProcessSpec, ProcessStream, StdinPolicy,
};

pub fn run_host_process<'arena>(
    spec: &ProcessSpec<'_>,
    caps: &ProcessCaps,
    arena: &'arena Arena,
) -> Result<ProcessResult<'arena>, ProcessError> {
    let mut command = Command::new(spec.program);
    command.args(spec.args.iter().map(ArenaString::as_str));
    if let Some(cwd) = spec.cwd {
        command.current_dir(cwd);
    }
    for pair in spec.env {
        command.env(pair.key.as_str(), pair.value.as_str());
    }

    configure_stdio(&mut command, spec);

    let mut child = command.spawn().map_err(ProcessError::SpawnFailed)?;
    let overflow = Arc::new(AtomicU8::new(0));

    let writer = spawn_stdin_writer(&mut child, spec.stdin);
    let stdout = spawn_capture_reader(
        &mut child,
        spec.stdout,
        caps.max_capture_bytes_per_stream,
        ProcessStream::Stdout,
        Arc::clone(&overflow),
    );
    let stderr = spawn_capture_reader(
        &mut child,
        spec.stderr,
        caps.max_capture_bytes_per_stream,
        ProcessStream::Stderr,
        Arc::clone(&overflow),
    );

    let status = match wait_for_child(&mut child, caps.wait_poll_ms, spec.timeout_ms, &overflow) {
        Ok(status) => status,
        Err(err) => {
            let _ = join_writer(writer);
            let _ = join_capture(stdout, ProcessStream::Stdout, &overflow, arena);
            let _ = join_capture(stderr, ProcessStream::Stderr, &overflow, arena);
            return Err(err);
        }
    };

    let writer_result = join_writer(writer);
    if let Err(err) = writer_result {
        return Err(ProcessError::SpawnFailed(err));
    }

    let stdout = join_capture(stdout, ProcessStream::Stdout, &overflow, arena)?;
    let stderr = join_capture(stderr, ProcessStream::Stderr, &overflow, arena)?;

    Ok(ProcessResult {
        success: status.code() == Some(0),
        exit_code: status.code(),
        stdout,
        stderr,
    })
}

fn configure_stdio(command: &mut Command, spec: &ProcessSpec<'_>) {
    command.stdin(match spec.stdin {
        StdinPolicy::Inherit => Stdio::inherit(),
        StdinPolicy::Null => Stdio::null(),
        StdinPolicy::Text(..) => Stdio::piped(),
    });
    command.stdout(output_stdio(spec.stdout));
    command.stderr(output_stdio(spec.stderr));
}

fn output_stdio(policy: OutputPolicy) -> Stdio {
    match policy {
        OutputPolicy::Inherit => Stdio::inherit(),
        OutputPolicy::Null => Stdio::null(),
        OutputPolicy::Capture => Stdio::piped(),
    }
}

fn spawn_stdin_writer(
    child: &mut Child,
    stdin: &StdinPolicy<'_>,
) -> Option<JoinHandle<io::Result<()>>> {
    let StdinPolicy::Text(text) = stdin else {
        return None;
    };
    let mut child_stdin = child.stdin.take()?;
    let input = text.as_bytes().to_vec();
    Some(thread::spawn(move || {
        if input.is_empty() {
            return Ok(());
        }
        match child_stdin.write_all(&input) {
            Ok(()) => Ok(()),
            Err(err) if err.kind() == io::ErrorKind::BrokenPipe => Ok(()),
            Err(err) => Err(err),
        }
    }))
}

fn spawn_capture_reader(
    child: &mut Child,
    policy: OutputPolicy,
    cap: u32,
    stream: ProcessStream,
    overflow: Arc<AtomicU8>,
) -> Option<JoinHandle<io::Result<std::vec::Vec<u8>>>> {
    if policy != OutputPolicy::Capture {
        return None;
    }

    let reader = match stream {
        ProcessStream::Stdout => child.stdout.take().map(StreamReader::Stdout),
        ProcessStream::Stderr => child.stderr.take().map(StreamReader::Stderr),
    }?;

    Some(thread::spawn(move || match reader {
        StreamReader::Stdout(stdout) => read_captured_stream(stdout, cap, 1, &overflow),
        StreamReader::Stderr(stderr) => read_captured_stream(stderr, cap, 2, &overflow),
    }))
}

enum StreamReader {
    Stdout(std::process::ChildStdout),
    Stderr(std::process::ChildStderr),
}

fn read_captured_stream<R: Read>(
    mut reader: R,
    cap: u32,
    overflow_code: u8,
    overflow: &Arc<AtomicU8>,
) -> io::Result<std::vec::Vec<u8>> {
    let mut buf = std::vec::Vec::with_capacity((cap.min(8_192)) as usize);
    let mut chunk = [0u8; 8_192];
    let max = cap as usize;

    loop {
        let n = reader.read(&mut chunk)?;
        if n == 0 {
            break;
        }
        if buf.len().saturating_add(n) > max {
            let _ = overflow.compare_exchange(0, overflow_code, Ordering::SeqCst, Ordering::SeqCst);
            break;
        }
        buf.extend_from_slice(&chunk[..n]);
    }

    Ok(buf)
}

fn wait_for_child(
    child: &mut Child,
    wait_poll_ms: u32,
    timeout_ms: u32,
    overflow: &AtomicU8,
) -> Result<std::process::ExitStatus, ProcessError> {
    let start = Instant::now();
    let sleep_for = Duration::from_millis(u64::from(wait_poll_ms.max(1)));
    let timeout = Duration::from_millis(u64::from(timeout_ms));

    loop {
        let overflow_code = overflow.load(Ordering::Acquire);
        if overflow_code != 0 {
            terminate_child(child);
            return Err(ProcessError::OutputLimitExceeded(stream_from_code(overflow_code)));
        }

        if let Some(status) = child.try_wait().map_err(ProcessError::SpawnFailed)? {
            return Ok(status);
        }
        if start.elapsed() >= timeout {
            terminate_child(child);
            return Err(ProcessError::Timeout);
        }
        thread::sleep(sleep_for);
    }
}

fn terminate_child(child: &mut Child) {
    let _ = child.kill();
    let _ = child.wait();
}

fn join_writer(writer: Option<JoinHandle<io::Result<()>>>) -> io::Result<()> {
    match writer {
        Some(handle) => handle.join().expect("stdin writer thread should not panic"),
        None => Ok(()),
    }
}

fn join_capture<'arena>(
    reader: Option<JoinHandle<io::Result<std::vec::Vec<u8>>>>,
    stream: ProcessStream,
    overflow: &AtomicU8,
    arena: &'arena Arena,
) -> Result<Option<ArenaString<'arena>>, ProcessError> {
    let Some(handle) = reader else {
        return Ok(None);
    };

    let bytes = handle
        .join()
        .expect("capture reader thread should not panic")
        .map_err(ProcessError::SpawnFailed)?;

    if overflow.load(Ordering::Acquire) == stream_code(stream) {
        return Err(ProcessError::OutputLimitExceeded(stream));
    }

    let text = String::from_utf8(bytes).map_err(|_| ProcessError::InvalidUtf8(stream))?;
    Ok(Some(ArenaString::from_str(arena, &text)))
}

const fn stream_code(stream: ProcessStream) -> u8 {
    match stream {
        ProcessStream::Stdout => 1,
        ProcessStream::Stderr => 2,
    }
}

const fn stream_from_code(code: u8) -> ProcessStream {
    match code {
        1 => ProcessStream::Stdout,
        _ => ProcessStream::Stderr,
    }
}
