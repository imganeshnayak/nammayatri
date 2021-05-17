module Types.Metrics
  ( module Types.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Monitoring.Prometheus.Metrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P

type TaskCounterMetric = P.Counter

type TaskDurationMetric = P.Histogram

type FailedTaskCounterMetric = P.Counter

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.counter $ P.Info "BTM_task_count" ""

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.counter $ P.Info "BTM_failed_task_count" ""

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20
