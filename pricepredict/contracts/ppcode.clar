;; Real Estate Price Prediction Market Contract - V1 (Basic)

;; Error Constants
(define-constant contract-owner tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-DOES-NOT-EXIST (err u102))
(define-constant ERR-PREDICTION-CLOSED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-ALREADY-SETTLED (err u105))
(define-constant ERR-INVALID-RANGE-COUNT (err u108))
(define-constant ERR-INVALID-CLOSE-HEIGHT (err u109))
(define-constant ERR-INVALID-PROPERTY-DETAILS (err u120))
(define-constant ERR-INVALID-PREDICTION-AMOUNT (err u121))

;; Data variables
(define-data-var next-property-id uint u0)

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
    prediction-close-height: uint
  }
)

;; Define user predictions structure
(define-map user-predictions
  { property-id: uint, predictor: principal }
  { chosen-range: uint, predicted-amount: uint }
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
(define-public (create-property-prediction (property-details (string-ascii 256)) (price-ranges (list 10 (string-ascii 64))) (prediction-close-height uint))
  (let
    (
      (new-property-id (var-get next-property-id))
    )
    (asserts! (> (len property-details) u0) ERR-INVALID-PROPERTY-DETAILS)
    (asserts! (> (len price-ranges) u1) ERR-INVALID-RANGE-COUNT)
    (asserts! (> prediction-close-height block-height) ERR-INVALID-CLOSE-HEIGHT)
    (map-set property-predictions
      { property-id: new-property-id }
      {
        creator: tx-sender,
        property-details: property-details,
        price-ranges: price-ranges,
        total-predicted-amount: u0,
        is-prediction-open: true,
        correct-range: u0,
        prediction-close-height: prediction-close-height
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
    (try! (as-contract (stx-transfer? (get predicted-amount user-pred) tx-sender tx-sender)))
    (map-delete user-predictions { property-id: property-id, predictor: tx-sender })
    (ok true)
  )
)

;; Contract initialization
(begin
  (var-set next-property-id u0)
)

;; Export the Component function (required for v0)
(define-public (Component)
  (ok true))