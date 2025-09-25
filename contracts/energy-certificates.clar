;; Renewable Energy Certificates Contract
;; Issue, track, and trade renewable energy certificates and carbon credits

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u402))
(define-constant ERR-INVALID-INPUT (err u403))
(define-constant ERR-ALREADY-EXISTS (err u404))
(define-constant ERR-INSUFFICIENT-BALANCE (err u405))
(define-constant ERR-EXPIRED (err u406))

;; Data Variables
(define-data-var next-certificate-id uint u1)
(define-data-var next-producer-id uint u1)
(define-data-var total-certificates uint u0)
(define-data-var total-energy-produced uint u0)

;; Data Maps
(define-map energy-producers
  { producer-id: uint }
  {
    owner: principal,
    facility-name: (string-ascii 100),
    energy-type: (string-ascii 50),
    capacity-mw: uint,
    location: (string-ascii 100),
    verified: bool,
    registration-date: uint
  }
)

(define-map energy-certificates
  { certificate-id: uint }
  {
    producer-id: uint,
    energy-amount: uint,
    generation-date: uint,
    issue-date: uint,
    valid-until: uint,
    owner: principal,
    retired: bool,
    carbon-offset: uint
  }
)

(define-map producer-balances
  { producer: principal }
  { balance: uint }
)

(define-map certificate-trades
  { certificate-id: uint, trade-id: uint }
  {
    seller: principal,
    buyer: principal,
    price: uint,
    trade-date: uint,
    completed: bool
  }
)

(define-map carbon-credits
  { credit-id: uint }
  {
    certificate-id: uint,
    co2-offset: uint,
    issue-date: uint,
    owner: principal,
    retired: bool
  }
)

;; Public Functions
(define-public (register-producer (facility-name (string-ascii 100)) (energy-type (string-ascii 50)) (capacity-mw uint) (location (string-ascii 100)))
  (let ((producer-id (var-get next-producer-id)))
    (asserts! (> (len facility-name) u0) ERR-INVALID-INPUT)
    (asserts! (> capacity-mw u0) ERR-INVALID-INPUT)
    
    (map-set energy-producers
      { producer-id: producer-id }
      {
        owner: tx-sender,
        facility-name: facility-name,
        energy-type: energy-type,
        capacity-mw: capacity-mw,
        location: location,
        verified: false,
        registration-date: stacks-block-height
      }
    )
    
    (var-set next-producer-id (+ producer-id u1))
    (ok producer-id)
  )
)

(define-public (verify-producer (producer-id uint))
  (let ((producer-data (unwrap! (map-get? energy-producers { producer-id: producer-id }) ERR-NOT-FOUND)))
    ;; In real implementation, this would be restricted to authorized verifiers
    (map-set energy-producers
      { producer-id: producer-id }
      (merge producer-data { verified: true })
    )
    
    (ok true)
  )
)

(define-public (issue-certificate (producer-id uint) (energy-amount uint) (generation-date uint))
  (let (
    (producer-data (unwrap! (map-get? energy-producers { producer-id: producer-id }) ERR-NOT-FOUND))
    (certificate-id (var-get next-certificate-id))
  )
    (asserts! (is-eq tx-sender (get owner producer-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get verified producer-data) ERR-NOT-AUTHORIZED)
    (asserts! (> energy-amount u0) ERR-INVALID-INPUT)
    
    (map-set energy-certificates
      { certificate-id: certificate-id }
      {
        producer-id: producer-id,
        energy-amount: energy-amount,
        generation-date: generation-date,
        issue-date: stacks-block-height,
        valid-until: (+ stacks-block-height u525600),
        owner: tx-sender,
        retired: false,
        carbon-offset: (/ energy-amount u2)
      }
    )
    
    (map-set producer-balances
      { producer: tx-sender }
      { balance: (+ (default-to u0 (get balance (map-get? producer-balances { producer: tx-sender }))) u1) }
    )
    
    (var-set next-certificate-id (+ certificate-id u1))
    (var-set total-certificates (+ (var-get total-certificates) u1))
    (var-set total-energy-produced (+ (var-get total-energy-produced) energy-amount))
    
    (ok certificate-id)
  )
)

(define-public (transfer-certificate (certificate-id uint) (new-owner principal))
  (let ((cert-data (unwrap! (map-get? energy-certificates { certificate-id: certificate-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner cert-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get retired cert-data)) ERR-INVALID-INPUT)
    
    (map-set energy-certificates
      { certificate-id: certificate-id }
      (merge cert-data { owner: new-owner })
    )
    
    (ok true)
  )
)

(define-public (retire-certificate (certificate-id uint) (retirement-reason (string-ascii 200)))
  (let ((cert-data (unwrap! (map-get? energy-certificates { certificate-id: certificate-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner cert-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get retired cert-data)) ERR-INVALID-INPUT)
    
    (map-set energy-certificates
      { certificate-id: certificate-id }
      (merge cert-data { retired: true })
    )
    
    (ok true)
  )
)

(define-public (create-trade-offer (certificate-id uint) (price uint))
  (let ((cert-data (unwrap! (map-get? energy-certificates { certificate-id: certificate-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner cert-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get retired cert-data)) ERR-INVALID-INPUT)
    (asserts! (> price u0) ERR-INVALID-INPUT)
    
    (map-set certificate-trades
      { certificate-id: certificate-id, trade-id: stacks-block-height }
      {
        seller: tx-sender,
        buyer: tx-sender,
        price: price,
        trade-date: stacks-block-height,
        completed: false
      }
    )
    
    (ok stacks-block-height)
  )
)

;; Read-only Functions
(define-read-only (get-producer (producer-id uint))
  (map-get? energy-producers { producer-id: producer-id })
)

(define-read-only (get-certificate (certificate-id uint))
  (map-get? energy-certificates { certificate-id: certificate-id })
)

(define-read-only (get-producer-balance (producer principal))
  (default-to u0 (get balance (map-get? producer-balances { producer: producer })))
)

(define-read-only (get-trade-offer (certificate-id uint) (trade-id uint))
  (map-get? certificate-trades { certificate-id: certificate-id, trade-id: trade-id })
)

(define-read-only (is-certificate-valid (certificate-id uint))
  (match (map-get? energy-certificates { certificate-id: certificate-id })
    cert-data
    (and (not (get retired cert-data))
         (> (get valid-until cert-data) stacks-block-height))
    false
  )
)

(define-read-only (calculate-carbon-credits (energy-amount uint))
  ;; Simple calculation: 1 MWh = 0.5 tons CO2 offset
  (/ energy-amount u2)
)

(define-read-only (get-system-stats)
  {
    total-certificates: (var-get total-certificates),
    total-energy-produced: (var-get total-energy-produced),
    next-certificate-id: (var-get next-certificate-id),
    next-producer-id: (var-get next-producer-id)
  }
)
