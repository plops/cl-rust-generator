use std::collections::VecDeque;
use std::time::{Duration, Instant};

const DEBOUNCE_WINDOW: Duration = Duration::from_millis(75);
const BANDWIDTH_BUDGET_BYTES_PER_SEC: usize = 10 * 1024; // 10KB/s
const BUDGET_WINDOW: Duration = Duration::from_secs(1);

/// Priority levels for egress messages
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Priority {
    Status = 0,    // Priority 1: status messages
    Content = 1,   // Priority 2: markdown structure
    Patch = 2,     // Priority 3: incremental DOM patches
    Metadata = 3,  // Priority 4: auxiliary data
}

/// A prioritized message ready for transmission
#[derive(Debug)]
pub struct PrioritizedMessage {
    pub priority: Priority,
    pub data: Vec<u8>,
    pub timestamp: Instant,
}

/// Debounces rapid CDP events over a configurable window
pub struct Debouncer {
    window: Duration,
    last_emit: Option<Instant>,
    pending: Vec<Vec<u8>>,
}

impl Debouncer {
    pub fn new() -> Self {
        Self {
            window: DEBOUNCE_WINDOW,
            last_emit: None,
            pending: Vec::new(),
        }
    }

    /// Push an event into the debouncer. Returns coalesced data if window elapsed.
    pub fn push(&mut self, data: Vec<u8>) -> Option<Vec<u8>> {
        self.pending.push(data);

        let now = Instant::now();
        match self.last_emit {
            Some(last) if now.duration_since(last) < self.window => None,
            _ => {
                self.last_emit = Some(now);
                // Coalesce: take only the latest pending event
                let result = self.pending.last().cloned();
                self.pending.clear();
                result
            }
        }
    }
}

/// Tracks bandwidth usage and enforces the 10KB/s budget
pub struct BandwidthBudget {
    bytes_sent: usize,
    window_start: Instant,
}

impl BandwidthBudget {
    pub fn new() -> Self {
        Self {
            bytes_sent: 0,
            window_start: Instant::now(),
        }
    }

    /// Check if we can send `size` bytes within budget. Resets window if elapsed.
    pub fn can_send(&mut self, size: usize) -> bool {
        let now = Instant::now();
        if now.duration_since(self.window_start) >= BUDGET_WINDOW {
            self.bytes_sent = 0;
            self.window_start = now;
        }
        self.bytes_sent + size <= BANDWIDTH_BUDGET_BYTES_PER_SEC
    }

    /// Record bytes sent
    pub fn record(&mut self, size: usize) {
        self.bytes_sent += size;
    }

    /// Returns utilization as a fraction (0.0 to 1.0)
    pub fn utilization(&self) -> f64 {
        self.bytes_sent as f64 / BANDWIDTH_BUDGET_BYTES_PER_SEC as f64
    }
}

/// Priority queue that drops low-priority messages under congestion
pub struct PriorityQueue {
    queues: [VecDeque<Vec<u8>>; 4],
    budget: BandwidthBudget,
}

impl PriorityQueue {
    pub fn new() -> Self {
        Self {
            queues: [
                VecDeque::new(),
                VecDeque::new(),
                VecDeque::new(),
                VecDeque::new(),
            ],
            budget: BandwidthBudget::new(),
        }
    }

    /// Enqueue a message at the given priority
    pub fn enqueue(&mut self, priority: Priority, data: Vec<u8>) {
        self.queues[priority as usize].push_back(data);
    }

    /// Dequeue the highest-priority message that fits the bandwidth budget.
    /// If utilization > 80%, drop Priority::Patch and Priority::Metadata.
    pub fn dequeue(&mut self) -> Option<(Priority, Vec<u8>)> {
        let congested = self.budget.utilization() > 0.8;

        for (i, queue) in self.queues.iter_mut().enumerate() {
            if congested && i >= Priority::Patch as usize {
                // Drop low-priority messages under congestion
                queue.clear();
                continue;
            }
            if let Some(data) = queue.front() {
                if self.budget.can_send(data.len()) {
                    let data = queue.pop_front().unwrap();
                    self.budget.record(data.len());
                    let priority = match i {
                        0 => Priority::Status,
                        1 => Priority::Content,
                        2 => Priority::Patch,
                        _ => Priority::Metadata,
                    };
                    return Some((priority, data));
                }
            }
        }
        None
    }
}
