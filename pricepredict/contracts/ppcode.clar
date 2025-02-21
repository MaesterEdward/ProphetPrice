;; Real Estate Price Prediction Market Contract - V3 (Complete)
;; Advanced version with comprehensive features for real estate price predictions

;; Error Constants
(define-constant contract-owner tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-DOES-NOT-EXIST (err u102))
(define-constant ERR-PREDICTION-CLOSED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-ALREADY-SETTLED (err u105))
(define-constant ERR-PREDICTION-NOT-CLOSABLE (err u106))
(define-constant ERR-PREDICTION-NOT-CANCELABLE (err u107))
(define-constant ERR-INVALID-RANGE-COUNT (err u108))
(define-constant ERR-INVALID-CLOSE-HEIGHT (err u109))
(define-constant ERR-INVALID-PREDICTION-TYPE (err u110))
(define-constant ERR-MISSING-MULTIPLIERS (err u111))
(define-constant ERR-REFUND-FAILED (err u118))
(define-constant ERR-INVALID-PROPERTY-DETAILS (err u120))
(define-constant ERR-INVALID-PREDICTION-AMOUNT (err u121))
(define-constant ERR-INVALID-LOCATION (err u122))
(define-constant ERR-INVALID-PROPERTY-TYPE (err u123))
(define-constant ERR-INVALID-MARKET-SEGMENT (err u124))
(define-constant ERR-INVALID-TIME-PERIOD (err u125))
(define-constant ERR-INVALID-MULTIPLIERS (err u126))
(define-constant ERR-INVALID-CHOSEN-RANGE (err u127))

;; Data variables
(define-data-var next-property-id uint u0)

;; Market configuration
(define-data-var prediction-types (list 10 (string-ascii 20)) 
  (list 
    "winner-takes-all"
    "weighted-share" 
    "fixed-multiplier"
  )
)

(define-data-var property-types (list 10 (string-ascii 20))
  (list
    "residential"
    "commercial"
    "industrial"
    "land"
    "mixed-use"
  )
)

(define-data-var market-segments (list 10 (string-ascii 20))
  (list
    "luxury"
    "mid-market"
    "affordable"
    "investment"
  )
)

;; Define enhanced property prediction structure
(define-map property-predictions
  { property-id: uint }
  {
    creator: principal,
    property-details: (string-ascii 256),
    property-type: (string-ascii 20),
    location: (string-ascii 100),
    price-ranges: (list 10 (string-ascii 64)),
    total-predicted-amount: uint,
    is-prediction-open: bool,
    correct-range: uint,
    prediction-close-height: uint,
    prediction-type: (string-ascii 20),
    market-segment: (string-ascii 20),
    multipliers: (optional (list 10 uint)),
    creation-time: uint,
    last-updated: uint,
    total-participants: uint
  }
)

;; Enhanced user predictions structure
(define-map user-predictions
  { property-id: uint, predictor: principal }
  {
    chosen-range: uint,
    predicted-amount: uint,
    prediction-time: uint,
    last-updated: uint,
    prediction-history: (list 5 uint)
  }
)

;; Market statistics
(define-map market-stats
  { market-segment: (string-ascii 20) }
  {
    total-predictions: uint,
    total-volume: uint,
    successful-predictions: uint,
    average-accuracy: uint
  }
)

;; Private validation functions
(define-private (validate-multipliers (mult (optional (list 10 uint))) (prediction-type (string-ascii 20)))
  (if (is-eq prediction-type "fixed-multiplier")
    (match mult
      multiplier-list (> (len multiplier-list) u0)
      false
    )
    true
  )
)

(define-private (validate-chosen-range (chosen uint) (ranges (list 10 (string-ascii 64))))
  (and 
    (> chosen u0)
    (<= chosen (len ranges))
  )
)

;; Private functions
(define-private (calculate-reward 
    (property 
      { 
        creator: principal, 
        property-details: (string-ascii 256),
        property-type: (string-ascii 20),
        location: (string-ascii 100),
        price-ranges: (list 10 (string-ascii 64)),
        total-predicted-amount: uint,
        is-prediction-open: bool,
        correct-range: uint,
        prediction-close-height: uint,
        prediction-type: (string-ascii 20),
        market-segment: (string-ascii 20),
        multipliers: (optional (list 10 uint)),
        creation-time: uint,
        last-updated: uint,
        total-participants: uint
      }
    )
    (user-pred { chosen-range: uint, predicted-amount: uint, prediction-time: uint, last-updated: uint, prediction-history: (list 5 uint) })
  )
  (let
    (
      (pred-type (get prediction-type property))
      (total-pool (get total-predicted-amount property))
      (user-amount (get predicted-amount user-pred))
    )
    (if (is-eq pred-type "winner-takes-all")
      total-pool
      (if (is-eq pred-type "weighted-share")
        (/ (* user-amount total-pool) total-pool)
        (let
          (
            (multiplier-list (unwrap! (get multipliers property) u0))
            (chosen-multiplier (unwrap! (element-at multiplier-list (- (get chosen-range user-pred) u1)) u0))
          )
          (+ user-amount (* user-amount (/ chosen-multiplier u100)))
        )
      )
    )
  )
)

(define-private (update-market-stats (market-segment (string-ascii 20)) (prediction-amount uint) (is-successful bool))
  (let
    (
      (current-stats (default-to 
        { total-predictions: u0, total-volume: u0, successful-predictions: u0, average-accuracy: u0 }
        (map-get? market-stats { market-segment: market-segment })))
    )
    (map-set market-stats
      { market-segment: market-segment }
      {
        total-predictions: (+ (get total-predictions current-stats) u1),
        total-volume: (+ (get total-volume current-stats) prediction-amount),
        successful-predictions: (+ (get successful-predictions current-stats) (if is-successful u1 u0)),
        average-accuracy: (if is-successful
          (/ (* (+ (get successful-predictions current-stats) u1) u100) (+ (get total-predictions current-stats) u1))
          (/ (* (get successful-predictions current-stats) u100) (+ (get total-predictions current-stats) u1)))
      }
    )
  )
)

(define-private (process-refund (property-id uint))
  (let
    ((user-pred (get-user-prediction property-id tx-sender)))
    (match user-pred
      pred-data (match (as-contract (stx-transfer? (get predicted-amount pred-data) tx-sender tx-sender))
        success (begin
          (map-delete user-predictions { property-id: property-id, predictor: tx-sender })
          (ok true)
        )
        error ERR-REFUND-FAILED
      )
      (ok true)
    )
  )
)

;; Read-only functions
(define-read-only (get-property-prediction (property-id uint))
  (map-get? property-predictions { property-id: property-id })
)

(define-read-only (get-user-prediction (property-id uint) (predictor principal))
  (map-get? user-predictions { property-id: property-id, predictor: predictor })
)

(define-read-only (get-market-statistics (market-segment (string-ascii 20)))
  (map-get? market-stats { market-segment: market-segment })
)

(define-read-only (get-current-block-height)
  block-height
)

;; Public functions
(define-public (create-property-prediction 
    (property-details (string-ascii 256))
    (property-type (string-ascii 20))
    (location (string-ascii 100))
    (price-ranges (list 10 (string-ascii 64)))
    (prediction-close-height uint)
    (prediction-type (string-ascii 20))
    (market-segment (string-ascii 20))
    (multipliers (optional (list 10 uint)))
  )
  (let
    (
      (new-property-id (var-get next-property-id))
    )
    (asserts! (> (len property-details) u0) ERR-INVALID-PROPERTY-DETAILS)
    (asserts! (> (len location) u0) ERR-INVALID-LOCATION)
    (asserts! (> (len price-ranges) u1) ERR-INVALID-RANGE-COUNT)
    (asserts! (> prediction-close-height block-height) ERR-INVALID-CLOSE-HEIGHT)
    (asserts! (is-some (index-of (var-get prediction-types) prediction-type)) ERR-INVALID-PREDICTION-TYPE)
    (asserts! (is-some (index-of (var-get property-types) property-type)) ERR-INVALID-PROPERTY-TYPE)
    (asserts! (is-some (index-of (var-get market-segments) market-segment)) ERR-INVALID-MARKET-SEGMENT)
    (asserts! (validate-multipliers multipliers prediction-type) ERR-INVALID-MULTIPLIERS)
    
    (map-set property-predictions
      { property-id: new-property-id }
      {
        creator: tx-sender,
        property-details: property-details,
        property-type: property-type,
        location: location,
        price-ranges: price-ranges,
        total-predicted-amount: u0,
        is-prediction-open: true,
        correct-range: u0,
        prediction-close-height: prediction-close-height,
        prediction-type: prediction-type,
        market-segment: market-segment,
        multipliers: multipliers,
        creation-time: block-height,
        last-updated: block-height,
        total-participants: u0
      }
    )
    (var-set next-property-id (+ new-property-id u1))
    (ok new-property-id)
  )
)

(define-public (make-prediction (property-id uint) (chosen-range uint) (prediction-amount uint))
  (let
    (
      (property (unwrap! (get-property-prediction property-id) ERR-DOES-NOT-EXIST))
      (existing-prediction (get-user-prediction property-id tx-sender))
    )
    (asserts! (> prediction-amount u0) ERR-INVALID-PREDICTION-AMOUNT)
    (asserts! (get is-prediction-open property) ERR-PREDICTION-CLOSED)
    (asserts! (validate-chosen-range chosen-range (get price-ranges property)) ERR-INVALID-CHOSEN-RANGE)
    (try! (stx-transfer? prediction-amount tx-sender (as-contract tx-sender)))
    
    ;; Update user prediction
    (map-set user-predictions
      { property-id: property-id, predictor: tx-sender }
      {
        chosen-range: chosen-range,
        predicted-amount: (+ prediction-amount (default-to u0 (get predicted-amount existing-prediction))),
        prediction-time: block-height,
        last-updated: block-height,
        prediction-history: (default-to (list) (get prediction-history existing-prediction))
      }
    )
    
    ;; Update property prediction
    (map-set property-predictions
      { property-id: property-id }
      (merge property
        {
          total-predicted-amount: (+ (get total-predicted-amount property) prediction-amount),
          last-updated: block-height,
          total-participants: (if (is-none existing-prediction)
            (+ (get total-participants property) u1)
            (get total-participants property))
        }
      )
    )
    (ok true)
  )
)

(define-public (cancel-prediction (property-id uint))
  (let
    (
      (property (unwrap! (get-property-prediction property-id) ERR-DOES-NOT-EXIST))
    )
    (asserts! (is-eq (get creator property) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (get is-prediction-open property) ERR-PREDICTION-CLOSED)
    (asserts! (< block-height (get prediction-close-height property)) ERR-PREDICTION-NOT-CANCELABLE)
    (map-set property-predictions
      { property-id: property-id }
      (merge property
        {
          is-prediction-open: false,
          last-updated: block-height
        }
      )
    )
    (process-refund property-id)
  )
)

(define-public (settle-property-prediction (property-id uint) (final-range uint))
  (let
    (
      (property (unwrap! (get-property-prediction property-id) ERR-DOES-NOT-EXIST))
    )
    (asserts! (is-eq contract-owner tx-sender) ERR-UNAUTHORIZED)
    (asserts! (get is-prediction-open property) ERR-PREDICTION-CLOSED)
    (asserts! (is-eq (get correct-range property) u0) ERR-ALREADY-SETTLED)
    (asserts! (validate-chosen-range final-range (get price-ranges property)) ERR-INVALID-CHOSEN-RANGE)
    
    ;; Update property prediction
    (map-set property-predictions
      { property-id: property-id }
      (merge property
        {
          is-prediction-open: false,
          correct-range: final-range,
          last-updated: block-height
        }
      )
    )
    
    ;; Update market statistics
    (update-market-stats (get market-segment property) (get total-predicted-amount property) true)
    (ok true)
  )
)

(define-public (claim-reward (property-id uint))
  (let
    (
      (property (unwrap! (get-property-prediction property-id) ERR-DOES-NOT-EXIST))
      (user-pred (unwrap! (get-user-prediction property-id tx-sender) ERR-DOES-NOT-EXIST))
    )
    (asserts! (is-eq (get chosen-range user-pred) (get correct-range property)) ERR-UNAUTHORIZED)
    (let
      (
        (reward (calculate-reward property user-pred))
      )
      (try! (as-contract (stx-transfer? reward tx-sender tx-sender)))
      (map-delete user-predictions { property-id: property-id, predictor: tx-sender })
      (ok reward)
    )
  )
)

;; Contract initialization
(begin
  (var-set next-property-id u0)
)

;; Export the Component