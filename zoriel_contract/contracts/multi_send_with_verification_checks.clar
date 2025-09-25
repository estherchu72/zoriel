;; Multi-Send Smart Contract with Verification Checks
;; This contract allows sending STX to multiple recipients with address validation

;; Error constants
(define-constant ERR-INVALID-RECIPIENT (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-EMPTY-RECIPIENT-LIST (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-TRANSFER-FAILED (err u104))
(define-constant ERR-UNAUTHORIZED (err u105))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map failed-transfers 
  { recipient: principal, amount: uint } 
  { failed-at-block: uint, reason: uint }
)

;; Helper function to validate recipient address
(define-private (is-valid-recipient (recipient principal))
  (and 
    ;; Check if recipient is not the zero address equivalent
    (not (is-eq recipient 'SP000000000000000000002Q6VF78))
    ;; Check if recipient is not the contract itself
    (not (is-eq recipient (as-contract tx-sender)))
    ;; Check if recipient is not the sender (prevent self-sends)
    (not (is-eq recipient tx-sender))
  )
)

;; Helper function to validate amount
(define-private (is-valid-amount (amount uint))
  (> amount u0)
)

;; Internal function to send STX to a single recipient with verification
(define-private (send-to-recipient (recipient-data { recipient: principal, amount: uint }))
  (let (
    (recipient (get recipient recipient-data))
    (amount (get amount recipient-data))
  )
    ;; Verify recipient address
    (if (not (is-valid-recipient recipient))
      (err ERR-INVALID-RECIPIENT)
      ;; Verify amount is valid
      (if (not (is-valid-amount amount))
        (err ERR-INVALID-AMOUNT)
        ;; Attempt to transfer STX
        (match (stx-transfer? amount tx-sender recipient)
          success (ok { recipient: recipient, amount: amount, status: "success" })
          error (begin
            ;; Log failed transfer
            (map-set failed-transfers 
              { recipient: recipient, amount: amount }
              { failed-at-block: block-height, reason: u1 }
            )
            (err ERR-TRANSFER-FAILED)
          )
        )
      )
    )
  )
)

;; Public function to send STX to multiple recipients
(define-public (multi-send (recipients (list 50 { recipient: principal, amount: uint })))
  (let (
    (total-amount (fold + (map get-amount recipients) u0))
    (sender-balance (stx-get-balance tx-sender))
  )
    ;; Check if recipient list is not empty
    (asserts! (> (len recipients) u0) ERR-EMPTY-RECIPIENT-LIST)
    
    ;; Check if sender has sufficient balance
    (asserts! (>= sender-balance total-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Process all transfers
    (ok (map send-to-recipient recipients))
  )
)

;; Helper function to extract amount from recipient data
(define-private (get-amount (recipient-data { recipient: principal, amount: uint }))
  (get amount recipient-data)
)

;; Public function for batch verification of recipient addresses
(define-public (verify-recipients (recipients (list 50 principal)))
  (let (
    (verification-results (map verify-single-recipient recipients))
  )
    (ok verification-results)
  )
)

;; Helper function to verify a single recipient
(define-private (verify-single-recipient (recipient principal))
  {
    recipient: recipient,
    is-valid: (is-valid-recipient recipient)
  }
)

;; Read-only function to check if a recipient address is valid
(define-read-only (is-recipient-valid (recipient principal))
  (is-valid-recipient recipient)
)

;; Read-only function to get failed transfer info
(define-read-only (get-failed-transfer (recipient principal) (amount uint))
  (map-get? failed-transfers { recipient: recipient, amount: amount })
)

;; Read-only function to calculate total amount for a list of recipients
(define-read-only (calculate-total-amount (recipients (list 50 { recipient: principal, amount: uint })))
  (fold + (map get-amount recipients) u0)
)

;; Emergency function to retrieve any STX accidentally sent to contract
(define-public (emergency-withdraw)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (let (
      (contract-balance (stx-get-balance (as-contract tx-sender)))
    )
      (as-contract (stx-transfer? contract-balance tx-sender CONTRACT-OWNER))
    )
  )
)

;; Public function with enhanced verification - validates all recipients before any transfers
(define-public (safe-multi-send (recipients (list 50 { recipient: principal, amount: uint })))
  (let (
    (total-amount (fold + (map get-amount recipients) u0))
    (sender-balance (stx-get-balance tx-sender))
    (all-recipients-valid (fold and (map validate-recipient-data recipients) true))
  )
    ;; Check if recipient list is not empty
    (asserts! (> (len recipients) u0) ERR-EMPTY-RECIPIENT-LIST)
    
    ;; Verify all recipients and amounts are valid before any transfers
    (asserts! all-recipients-valid ERR-INVALID-RECIPIENT)
    
    ;; Check if sender has sufficient balance
    (asserts! (>= sender-balance total-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Process all transfers only after all validations pass
    (ok (map send-to-recipient recipients))
  )
)

;; Helper function to validate recipient data structure
(define-private (validate-recipient-data (recipient-data { recipient: principal, amount: uint }))
  (and 
    (is-valid-recipient (get recipient recipient-data))
    (is-valid-amount (get amount recipient-data))
  )
)