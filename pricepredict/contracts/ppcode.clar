;; Real Estate Price Prediction Market Contract - V2

;; Error Constants
(define-constant contract-owner tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-DOES-NOT-EXIST (err u102))
(define-constant ERR-PREDICTION-CLOSED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-ALREADY-SETTLED (err u105))
(define-constant ERR-PREDICTION-NOT-CANCELABLE (err u107))
(define-constant ERR-INVALID-RANGE-COUNT (err u108))
(define-constant ERR-INVALID-CLOSE-HEIGHT (err u109))
(define-constant ERR-INVALID-PREDICTION-TYPE (err u110))
(define-constant ERR-REFUND-FAILED (err u118))
(define-constant ERR-INVALID-PROPERTY-DETAILS (err u120))
(define-constant ERR-INVALID-PREDICTION-AMOUNT (err u121))

;; Data variables
(define-data-var next-property-id uint u0)

;; Prediction types
(define-data-var prediction-types (list 10 (string-ascii 20)) (list "bulk-residential" "luxury-market"))

;; Define property prediction structure
(define-map property-predictions
  { property-id: uint }
  {
    creator: principal,
    property-details: (string-ascii 256),
    price-ranges: (list 10 (string-ascii 64)),
    total-predicted-amount: uint,
    is-prediction-open: bool,
    correct-range: uint,
    prediction-close-height: uint,
    prediction-type: (string-ascii 20),
    market-segment: (string-ascii 20)
  }
)

;; Define user predictions structure
(define-map user-predictions
  { property-id: uint, predictor: principal }
  { chosen-range: uint, predicted-amount: uint }
)

;; Private functions
(define-private (calculate-reward (property { creator: principal, property-details: (string-ascii 256), price-ranges: (list 10 (string-ascii 64)), total-predicted-amount: uint, is-prediction-open: bool, correct-range: uint, prediction-close-height: uint, prediction-type: (string-ascii 20), market-segment: (string-ascii 20) }) (user-pred { chosen-range: uint, predicted-amount: uint }))
  (let
    (
      (pred-type (get prediction-type property))
      (total-pool (get total-predicted-amount property))
      (user-amount (get predicted-amount user-pred))
    )
    (if (is-eq pred-type "bulk-residential")
      total-pool
      (/ (* user-amount total-pool) total-pool)
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

(define-read-only (get-current-block-height)
  block-height
)

;; Public functions
(define-public (create-property-prediction (property-details (string-ascii 256)) (price-ranges (list 10 (string-ascii 64))) (prediction-close-height uint) (prediction-type (string-ascii 20)) (market-segment (string-ascii 20)))
  (let
    (
      (new-property-id (var-get next-property-id))
    )
    (asserts! (> (len property-details) u0) ERR-INVALID-PROPERTY-DETAILS)
    (asserts! (> (len price-ranges) u1) ERR-INVALID-RANGE-COUNT)
    (asserts! (> prediction-close-height block-height) ERR-INVALID-CLOSE-HEIGHT)
    (asserts! (is-some (index-of (var-get prediction-types) prediction-type)) ERR-INVALID-PREDICTION-TYPE)
    (map-set property-predictions
      { property-id: new-property-id }
      {
        creator: tx-sender,
        property-details: property-details,
        price-ranges: price-ranges,
        total-predicted-amount: u0,
        is-prediction-open: true,
        correct-range: u0,
        prediction-close-height: prediction-close-height,
        prediction-type: prediction-type,
        market-segment: market-segment
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
    )
    (asserts! (> prediction-amount u0) ERR-INVALID-PREDICTION-AMOUNT)
    (asserts! (get is-prediction-open property) ERR-PREDICTION-CLOSED)
    (try! (stx-transfer? prediction-amount tx-sender (as-contract tx-sender)))
    (map-set user-predictions
      { property-id: property-id, predictor: tx-sender }
      { chosen-range: chosen-range, predicted-amount: prediction-amount }
    )
    (map-set property-predictions
      { property-id: property-id }
      (merge property { total-predicted-amount: (+ (get total-predicted-amount property) prediction-amount) })
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
      (merge property { is-prediction-open: false })
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
    (map-set property-predictions
      { property-id: property-id }
      (merge property { is-prediction-open: false, correct-range: final-range })
    )
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

;; Export the Component function (required for v0)
(define-public (Component)
  (ok true))