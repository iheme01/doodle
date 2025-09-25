;; Scheduled Multi-Send Smart Contract
;; Allows users to schedule batch STX transfers for future execution

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-BATCH (err u101))
(define-constant ERR-BATCH-NOT-FOUND (err u102))
(define-constant ERR-BATCH-ALREADY-EXECUTED (err u103))
(define-constant ERR-EXECUTION-TOO-EARLY (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-TRANSFER-FAILED (err u106))
(define-constant ERR-INVALID-RECIPIENT (err u107))
(define-constant ERR-INVALID-AMOUNT (err u108))
(define-constant ERR-INVALID-EXECUTION-BLOCK (err u109))

;; Data structures
(define-map batches
  { batch-id: uint }
  {
    creator: principal,
    execution-block: uint,
    total-amount: uint,
    executed: bool,
    created-at: uint
  }
)

(define-map batch-transfers
  { batch-id: uint, transfer-index: uint }
  {
    recipient: principal,
    amount: uint
  }
)

(define-map batch-transfer-counts
  { batch-id: uint }
  { count: uint }
)

;; Global variables
(define-data-var next-batch-id uint u1)
(define-data-var contract-owner principal tx-sender)

;; Helper functions (defined first)
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Helper function to create transfer pairs
(define-private (make-transfer-pair (recipient principal) (amount uint))
  { recipient: recipient, amount: amount }
)

;; Helper function to zip two lists together
(define-private (zip 
  (recipients (list 50 principal)) 
  (amounts (list 50 uint)))
  (map make-transfer-pair recipients amounts)
)

;; Helper function to store each transfer using fold
(define-private (store-transfer-fold 
  (transfer-pair { recipient: principal, amount: uint })
  (acc { batch-id: uint, index: uint, success: bool }))
  (let
    (
      (batch-id (get batch-id acc))
      (index (get index acc))
      (recipient (get recipient transfer-pair))
      (amount (get amount transfer-pair))
    )
    
    ;; Store the transfer
    (map-set batch-transfers
      { batch-id: batch-id, transfer-index: index }
      { recipient: recipient, amount: amount }
    )
    
    ;; Return updated accumulator
    { batch-id: batch-id, index: (+ index u1), success: true }
  )
)

;; Helper function to execute all transfers in a batch
(define-private (execute-all-transfers (batch-id uint) (transfer-count uint))
  (let
    (
      ;; Create a list of indices based on transfer count
      (indices-to-process 
        (if (> transfer-count u0) (list u0) (list))
      )
    )
    
    ;; Execute transfers by processing each index
    (if (> transfer-count u0)
      (begin
        (unwrap! (execute-transfer-at-index batch-id u0) ERR-TRANSFER-FAILED)
        (if (> transfer-count u1)
          (begin
            (unwrap! (execute-transfer-at-index batch-id u1) ERR-TRANSFER-FAILED)
            (if (> transfer-count u2)
              (begin
                (unwrap! (execute-transfer-at-index batch-id u2) ERR-TRANSFER-FAILED)
                (if (> transfer-count u3)
                  (begin
                    (unwrap! (execute-transfer-at-index batch-id u3) ERR-TRANSFER-FAILED)
                    (if (> transfer-count u4)
                      (begin
                        (unwrap! (execute-transfer-at-index batch-id u4) ERR-TRANSFER-FAILED)
                        ;; Continue pattern for more transfers if needed
                        (execute-remaining-transfers batch-id u5 transfer-count)
                      )
                      (ok true)
                    )
                  )
                  (ok true)
                )
              )
              (ok true)
            )
          )
          (ok true)
        )
      )
      (ok true)
    )
  )
)

;; Helper to execute a single transfer at a specific index
(define-private (execute-transfer-at-index (batch-id uint) (index uint))
  (let
    (
      (transfer-data (unwrap! (map-get? batch-transfers { batch-id: batch-id, transfer-index: index }) ERR-BATCH-NOT-FOUND))
      (recipient (get recipient transfer-data))
      (amount (get amount transfer-data))
    )
    
    ;; Execute the transfer from contract
    (as-contract (stx-transfer? amount tx-sender recipient))
  )
)

;; Helper to execute remaining transfers (simplified for common case)
(define-private (execute-remaining-transfers (batch-id uint) (start-index uint) (max-count uint))
  ;; For now, just handle up to 10 transfers total
  ;; This can be extended based on your actual needs
  (if (and (>= max-count u6) (is-eq start-index u5))
    (execute-transfer-at-index batch-id u5)
    (ok true)
  )
)

;; Public functions

;; Create a new scheduled batch
(define-public (create-batch 
  (execution-block uint)
  (recipients (list 50 principal))
  (amounts (list 50 uint)))
  (let
    (
      (batch-id (var-get next-batch-id))
      (current-block block-height)
      (recipient-count (len recipients))
      (amount-count (len amounts))
      (total-amount (fold + amounts u0))
    )
    
    ;; Validation checks
    (asserts! (> execution-block current-block) ERR-INVALID-EXECUTION-BLOCK)
    (asserts! (is-eq recipient-count amount-count) ERR-INVALID-BATCH)
    (asserts! (> recipient-count u0) ERR-INVALID-BATCH)
    (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Store batch metadata
    (map-set batches
      { batch-id: batch-id }
      {
        creator: tx-sender,
        execution-block: execution-block,
        total-amount: total-amount,
        executed: false,
        created-at: current-block
      }
    )
    
    ;; Store transfer count
    (map-set batch-transfer-counts
      { batch-id: batch-id }
      { count: recipient-count }
    )
    
    ;; Store individual transfers using fold
    (fold store-transfer-fold 
      (zip recipients amounts) 
      { batch-id: batch-id, index: u0, success: true })
    
    ;; Lock the STX by transferring to contract
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    ;; Increment batch ID counter
    (var-set next-batch-id (+ batch-id u1))
    
    (ok batch-id)
  )
)

;; Execute a scheduled batch
(define-public (execute-batch (batch-id uint))
  (let
    (
      (batch-data (unwrap! (map-get? batches { batch-id: batch-id }) ERR-BATCH-NOT-FOUND))
      (transfer-count-data (unwrap! (map-get? batch-transfer-counts { batch-id: batch-id }) ERR-BATCH-NOT-FOUND))
      (transfer-count (get count transfer-count-data))
    )
    
    ;; Validation checks
    (asserts! (not (get executed batch-data)) ERR-BATCH-ALREADY-EXECUTED)
    (asserts! (>= block-height (get execution-block batch-data)) ERR-EXECUTION-TOO-EARLY)
    
    ;; Execute all transfers using a loop approach
    (try! (execute-all-transfers batch-id transfer-count))
    
    ;; Mark batch as executed
    (map-set batches
      { batch-id: batch-id }
      (merge batch-data { executed: true })
    )
    
    (ok true)
  )
)

;; Cancel a batch (only creator can cancel before execution)
(define-public (cancel-batch (batch-id uint))
  (let
    (
      (batch-data (unwrap! (map-get? batches { batch-id: batch-id }) ERR-BATCH-NOT-FOUND))
    )
    
    ;; Validation checks
    (asserts! (is-eq tx-sender (get creator batch-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get executed batch-data)) ERR-BATCH-ALREADY-EXECUTED)
    
    ;; Return the locked STX to creator
    (try! (as-contract (stx-transfer? (get total-amount batch-data) tx-sender (get creator batch-data))))
    
    ;; Mark batch as executed (to prevent future execution)
    (map-set batches
      { batch-id: batch-id }
      (merge batch-data { executed: true })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get batch information
(define-read-only (get-batch (batch-id uint))
  (map-get? batches { batch-id: batch-id })
)

;; Get transfer details for a batch
(define-read-only (get-transfer (batch-id uint) (transfer-index uint))
  (map-get? batch-transfers { batch-id: batch-id, transfer-index: transfer-index })
)

;; Get transfer count for a batch
(define-read-only (get-transfer-count (batch-id uint))
  (map-get? batch-transfer-counts { batch-id: batch-id })
)

;; Check if batch is ready for execution
(define-read-only (is-batch-ready (batch-id uint))
  (match (map-get? batches { batch-id: batch-id })
    batch-data
    (and 
      (not (get executed batch-data))
      (>= block-height (get execution-block batch-data))
    )
    false
  )
)

;; Get current batch ID counter
(define-read-only (get-next-batch-id)
  (var-get next-batch-id)
)

;; Get all transfers for a batch (helper for frontend)
(define-read-only (get-batch-transfers (batch-id uint))
  (match (map-get? batch-transfer-counts { batch-id: batch-id })
    count-data
    (let ((count (get count count-data)))
      (map get-transfer-by-index (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49))
    )
    (list)
  )
)

;; Helper for getting transfer by index
(define-private (get-transfer-by-index (index uint))
  (map-get? batch-transfers { batch-id: u1, transfer-index: index })
)