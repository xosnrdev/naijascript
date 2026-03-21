//! Central analysis limits and preflight checks.

use crate::analysis::cfg::ProgramCounts;
use crate::analysis::facts::ProgramFacts;

/// Centralized analysis caps used to bound work and memory growth.
#[derive(Debug, Clone, Copy)]
pub struct AnalysisCaps {
    pub max_functions: u32,
    pub max_locals: u32,
    pub max_scopes: u32,
    pub max_statements: u32,
    pub max_total_blocks: u32,
    pub max_blocks_per_function: u32,
    pub max_direct_user_calls: u32,
    pub max_summary_events: u64,
    pub max_liveness_events: u64,
}

/// Default analysis caps for one interpreter run.
pub const DEFAULT_CAPS: AnalysisCaps = AnalysisCaps {
    max_functions: 16_384,
    max_locals: 131_072,
    max_scopes: 131_072,
    max_statements: 262_144,
    max_total_blocks: 524_288,
    max_blocks_per_function: 65_536,
    max_direct_user_calls: 262_144,
    max_summary_events: 16_777_216,
    max_liveness_events: 33_554_432,
};

/// Single exceeded analysis limit. The first hit stops the staged analyses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AnalysisLimit {
    pub metric: &'static str,
    pub observed: u64,
    pub limit: u64,
}

impl AnalysisLimit {
    /// Returns a source-oriented message describing the exceeded cap.
    #[must_use]
    pub const fn message(self) -> &'static str {
        "Analysis skipped after reaching a configured resource limit"
    }
}

/// Validates program facts and precomputed CFG counts against the configured caps.
#[must_use]
pub fn first_exceeded_limit(
    facts: &ProgramFacts<'_, '_>,
    counts: &ProgramCounts<'_>,
    caps: AnalysisCaps,
) -> Option<AnalysisLimit> {
    let function_count = facts.functions.len() as u64;
    if function_count > u64::from(caps.max_functions) {
        return Some(AnalysisLimit {
            metric: "functions",
            observed: function_count,
            limit: u64::from(caps.max_functions),
        });
    }

    let local_count = facts.locals.len() as u64;
    if local_count > u64::from(caps.max_locals) {
        return Some(AnalysisLimit {
            metric: "locals",
            observed: local_count,
            limit: u64::from(caps.max_locals),
        });
    }

    let scope_count = facts.scopes.len() as u64;
    if scope_count > u64::from(caps.max_scopes) {
        return Some(AnalysisLimit {
            metric: "scopes",
            observed: scope_count,
            limit: u64::from(caps.max_scopes),
        });
    }

    let statement_count = facts.stmt_effects.len() as u64;
    if statement_count > u64::from(caps.max_statements) {
        return Some(AnalysisLimit {
            metric: "statements",
            observed: statement_count,
            limit: u64::from(caps.max_statements),
        });
    }

    let total_blocks = u64::from(counts.total_blocks);
    if total_blocks > u64::from(caps.max_total_blocks) {
        return Some(AnalysisLimit {
            metric: "cfg blocks",
            observed: total_blocks,
            limit: u64::from(caps.max_total_blocks),
        });
    }

    if let Some(blocks_per_function) = counts.function_blocks.iter().copied().map(u64::from).max()
        && blocks_per_function > u64::from(caps.max_blocks_per_function)
    {
        return Some(AnalysisLimit {
            metric: "blocks in one function",
            observed: blocks_per_function,
            limit: u64::from(caps.max_blocks_per_function),
        });
    }

    let direct_user_calls = facts.user_calls.len() as u64;
    if direct_user_calls > u64::from(caps.max_direct_user_calls) {
        return Some(AnalysisLimit {
            metric: "direct user calls",
            observed: direct_user_calls,
            limit: u64::from(caps.max_direct_user_calls),
        });
    }

    let summary_events = summary_event_bound(facts);
    if summary_events > caps.max_summary_events {
        return Some(AnalysisLimit {
            metric: "summary events",
            observed: summary_events,
            limit: caps.max_summary_events,
        });
    }

    let liveness_events = liveness_event_bound(facts, counts);
    if liveness_events > caps.max_liveness_events {
        return Some(AnalysisLimit {
            metric: "liveness events",
            observed: liveness_events,
            limit: caps.max_liveness_events,
        });
    }

    None
}

fn summary_event_bound(facts: &ProgramFacts<'_, '_>) -> u64 {
    let function_count = facts.functions.len() as u64;
    let local_count = facts.locals.len() as u64;

    // Each function summary can only grow across callees, capture reads, capture writes,
    // and two class escalations.
    function_count.saturating_mul(function_count.saturating_add(local_count.saturating_mul(2) + 2))
}

fn liveness_event_bound(facts: &ProgramFacts<'_, '_>, counts: &ProgramCounts<'_>) -> u64 {
    counts.function_blocks.iter().enumerate().fold(0_u64, |events, (function_idx, block_count)| {
        let function = crate::analysis::ids::FunctionId(function_idx as u32);
        let local_range = facts.local_range(function);
        let local_count = u64::from(local_range.end - local_range.start);
        events.saturating_add(u64::from(*block_count).saturating_mul(local_count))
    })
}
