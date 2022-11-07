(ns timer-clj
  (:import [java.util Timer TimerTask Date]))
(defn timer
  "Create a new java.util.Timer object."
  {:added "1.0.0"}
  [& {:keys [name ^boolean is-daemon] :or {is-daemon false}}]
  {:pre [(or (nil? name) (instance? String name))
         (instance? Boolean is-daemon)]
   :post [(instance? Timer %)]}
  (if name
    (new Timer name is-daemon)
    (new Timer is-daemon)))

(defn timer-task
  "Create a new TimerTask object.
  Adding a task with a timer-task allows canceling the submitted task, without
  canceling the entire timer that the task is being run upon."
  {:added "1.0.2"}
  [task & {:keys [on-exception]}]
  {:pre [(fn? task)
         (or (nil? on-exception) (fn? on-exception))]
   :post [(instance? TimerTask %)]}
  (proxy [TimerTask] []
    (run []
      (try
        (task)
        (catch Exception e
          (if on-exception
            (on-exception e)
            (throw e)))))))

(defn run-task!
  "Execute a timer task, then return the timer user passed or be auto created.
  Normally, User need set one of the two options:
    :at <time>
    :delay <milliseconds>
  If set none of them, the task will launch immediately.
  Optional, user can set:
    :period <milliseconds>
  To reuse a timer between two or more tasks, pass in an existing timer:
    :by <a timer>
  Sometimes user task may cause exception, it's a good reason to use an exception handler:
    :on-exception <handle function with an exception argument>"
  {:added "1.0.0"}
  [task & {:keys [by, ^Date at, ^long delay, ^long period, on-exception]
           :or {delay 0}}]
  {:pre [(or (nil? by) (instance? Timer by))
         (or (nil? at) (instance? Date at))
         (or (nil? delay) (and (instance? Long delay) (>= delay 0)))
         (or (nil? period) (and (instance? Long period) (pos? period)))]
   :post [(instance? Timer %)]}
  (let [timer-task (if (instance? TimerTask task)
                     task
                     (timer-task task :on-exception on-exception))
        ^Timer target-timer (or by (timer))]
    (if at
      (if (nil? period)
        (.schedule target-timer ^TimerTask timer-task at)
        (.schedule target-timer ^TimerTask timer-task at period))
      (if (nil? period)
        (.schedule target-timer ^TimerTask timer-task delay)
        (.schedule target-timer ^TimerTask timer-task delay period)))
    target-timer))

(defn cancel!
  "Terminates a timer, discarding any currently scheduled tasks."
  {:added "1.0.0"}
  [^Timer timer]
  (.cancel timer))

(defn cancel-task!
  "Terminates a timer task"
  {:added "1.0.2"}
  [^TimerTask task]
  (.cancel task))