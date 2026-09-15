use crate::framework::{
    environment::{Analysis, Context, Location},
    erased::{Active, Factory, Snapshot},
    instruction::AnalysisInstruction,
};
use crate::{
    MIRAnalysisOptions,
    passes::{Ownership, ValueTracking},
};
use cx_log::CXResult;
use cx_mir::{MIRBlockTarget, MIRBody, MIRFunctionBody, MIRUnit, visit::MIRVisitor};
use std::{collections::VecDeque, convert::Infallible};

pub struct Pipeline {
    analyses: Vec<Box<dyn Factory>>,
}

impl Pipeline {
    pub fn new(options: MIRAnalysisOptions) -> Self {
        let mut pipeline = Self {
            analyses: Vec::new(),
        };
        if options.ownership {
            pipeline.push(Ownership);
        }
        if options.values {
            pipeline.push(ValueTracking);
        }
        pipeline
    }

    pub fn push<A: Analysis>(&mut self, analysis: A) {
        self.analyses.push(Box::new(analysis));
    }

    pub fn analyze(&self, unit: &MIRUnit) -> CXResult<()> {
        if self.analyses.is_empty() {
            return Ok(());
        }
        for function in unit.functions() {
            let context = Context { unit, function };
            let mut active: Vec<_> = self
                .analyses
                .iter()
                .filter_map(|analysis| analysis.create(&context))
                .collect();
            if active.is_empty() {
                continue;
            }
            match function.body() {
                Some(MIRFunctionBody::Runtime(body)) => run(&context, body, &mut active)?,
                Some(MIRFunctionBody::Comptime(body)) => run(&context, body, &mut active)?,
                None => {}
            }
        }
        Ok(())
    }
}

type Snapshots = Vec<Box<dyn Snapshot>>;

fn snapshot(active: &[Box<dyn Active>]) -> Snapshots {
    active.iter().map(|analysis| analysis.snapshot()).collect()
}

fn restore(active: &mut [Box<dyn Active>], states: &Snapshots) {
    for (analysis, state) in active.iter_mut().zip(states) {
        analysis.restore(state.as_ref());
    }
}

fn run<K: AnalysisInstruction>(
    context: &Context<'_>,
    body: &MIRBody<K>,
    active: &mut [Box<dyn Active>],
) -> CXResult<()> {
    let entry = body.entry().index();
    if entry >= body.blocks().len() {
        return Ok(());
    }
    let mut entries: Vec<Option<Snapshots>> = (0..body.blocks().len()).map(|_| None).collect();
    entries[entry] = Some(snapshot(active));
    let edges: Vec<_> = body
        .blocks()
        .iter()
        .map(|block| {
            let mut targets = Targets(Vec::new());
            if let Some(instruction) = block.instrs.last() {
                let Ok(()) = instruction.visit(&mut targets);
            }
            targets.0
        })
        .collect();
    let mut pending = VecDeque::from([entry]);
    let mut queued = vec![false; entries.len()];
    queued[entry] = true;
    while let Some(index) = pending.pop_front() {
        queued[index] = false;
        restore(
            active,
            entries[index].as_ref().expect("queued block is reachable"),
        );
        transfer(context, body, index, active, false)?;
        if edges[index].is_empty() {
            continue;
        }
        let outgoing = snapshot(active);
        let block = &body.blocks()[index];
        let location = Location {
            block: block.id,
            instruction: block.instrs.len().saturating_sub(1),
        };
        for target in &edges[index] {
            let destination = body
                .block(target.block)
                .expect("MIR edge has no destination");
            restore(active, &outgoing);
            for analysis in active.iter_mut() {
                analysis.edge(context, location, &target.args, &destination.params, false)?;
            }
            let incoming = snapshot(active);
            let changed = match &mut entries[target.block.index()] {
                Some(existing) => {
                    let mut changed = false;
                    for (state, incoming) in existing.iter_mut().zip(&incoming) {
                        changed |= state.merge(incoming.as_ref());
                    }
                    changed
                }
                slot @ None => {
                    *slot = Some(incoming);
                    true
                }
            };
            if changed && !queued[target.block.index()] {
                queued[target.block.index()] = true;
                pending.push_back(target.block.index());
            }
        }
    }
    for (index, entry) in entries.iter().enumerate() {
        let Some(entry) = entry else {
            continue;
        };
        restore(active, entry);
        let block = &body.blocks()[index];
        for analysis in active.iter() {
            analysis.validate(
                context,
                Location {
                    block: block.id,
                    instruction: 0,
                },
            )?;
        }
        transfer(context, body, index, active, true)?;
        if edges[index].is_empty() {
            continue;
        }
        let outgoing = snapshot(active);
        let location = Location {
            block: block.id,
            instruction: block.instrs.len().saturating_sub(1),
        };
        for target in &edges[index] {
            let destination = body
                .block(target.block)
                .expect("MIR edge has no destination");
            restore(active, &outgoing);
            for analysis in active.iter_mut() {
                analysis.edge(context, location, &target.args, &destination.params, true)?;
            }
        }
    }
    Ok(())
}

fn transfer<K: AnalysisInstruction>(
    context: &Context<'_>,
    body: &MIRBody<K>,
    index: usize,
    active: &mut [Box<dyn Active>],
    diagnose: bool,
) -> CXResult<()> {
    let block = &body.blocks()[index];
    for (index, instruction) in block.instrs.iter().enumerate() {
        let location = Location {
            block: block.id,
            instruction: index,
        };
        let instruction = instruction.kind.view();
        for analysis in active.iter_mut() {
            analysis.instruction(context, location, instruction, diagnose)?;
        }
    }
    Ok(())
}

struct Targets<'ir>(Vec<&'ir MIRBlockTarget>);

impl<'ir> MIRVisitor<'ir> for Targets<'ir> {
    type Error = Infallible;
    fn target(&mut self, target: &'ir MIRBlockTarget) -> Result<(), Infallible> {
        self.0.push(target);
        Ok(())
    }
}
