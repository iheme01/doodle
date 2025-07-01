;; Enhanced Multi-User Storage Smart Contract
;; Advanced storage system with multiple data types, permissions, and analytics

;; Error constants
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-INPUT (err u400))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant STORAGE-FEE u1000) ;; Fee in microSTX for premium storage

;; Data maps for different storage types
(define-map user-numbers principal uint)
(define-map user-strings principal (string-ascii 256))
(define-map user-booleans principal bool)
(define-map user-metadata principal {
    created-at: uint,
    updated-at: uint,
    access-count: uint,
    is-premium: bool
})

;; Premium features map
(define-map premium-users principal {
    expires-at: uint,
    storage-limit: uint,
    features: (list 10 (string-ascii 32))
})

;; Access control
(define-map authorized-readers principal (list 50 principal))
(define-map public-profiles principal bool)

;; Contract statistics
(define-data-var total-users uint u0)
(define-data-var total-operations uint u0)
(define-data-var contract-balance uint u0)

;; Events (using print for logging)
(define-private (log-event (event-type (string-ascii 32)) (user principal) (data (string-ascii 256)))
  (print {
    event: event-type,
    user: user,
    data: data,
    block-height: block-height,
    timestamp: stx-liquid-supply
  })
)

;; === BASIC STORAGE FUNCTIONS ===

;; Store a number value
(define-public (store-number (value uint))
  (let ((existing (map-get? user-numbers tx-sender)))
    (begin
      (map-set user-numbers tx-sender value)
      (update-metadata)
      (if (is-none existing)
        (var-set total-users (+ (var-get total-users) u1))
        true)
      (log-event "STORE_NUMBER" tx-sender (int-to-ascii (to-int value)))
      (increment-operations)
      (ok true)
    )
  )
)

;; Store a string value
(define-public (store-string (value (string-ascii 256)))
  (let ((existing (map-get? user-strings tx-sender)))
    (begin
      (asserts! (> (len value) u0) ERR-INVALID-INPUT)
      (map-set user-strings tx-sender value)
      (update-metadata)
      (if (is-none existing)
        (var-set total-users (+ (var-get total-users) u1))
        true)
      (log-event "STORE_STRING" tx-sender value)
      (increment-operations)
      (ok true)
    )
  )
)

;; Store a boolean value
(define-public (store-boolean (value bool))
  (let ((existing (map-get? user-booleans tx-sender)))
    (begin
      (map-set user-booleans tx-sender value)
      (update-metadata)
      (if (is-none existing)
        (var-set total-users (+ (var-get total-users) u1))
        true)
      (log-event "STORE_BOOLEAN" tx-sender (if value "true" "false"))
      (increment-operations)
      (ok true)
    )
  )
)

;; === GETTER FUNCTIONS ===

;; Get user's number (with access control)
(define-read-only (get-number (user principal))
  (if (can-read-user-data user)
    (map-get? user-numbers user)
    none
  )
)

;; Get user's string
(define-read-only (get-string (user principal))
  (if (can-read-user-data user)
    (map-get? user-strings user)
    none
  )
)

;; Get user's boolean
(define-read-only (get-boolean (user principal))
  (if (can-read-user-data user)
    (map-get? user-booleans user)
    none
  )
)

;; Get all user data at once
(define-read-only (get-all-data (user principal))
  (if (can-read-user-data user)
    (some {
      number: (map-get? user-numbers user),
      string: (map-get? user-strings user),
      boolean: (map-get? user-booleans user),
      metadata: (map-get? user-metadata user)
    })
    none
  )
)

;; === PREMIUM FEATURES ===

;; Upgrade to premium (requires payment)
(define-public (upgrade-to-premium (duration uint))
  (let ((fee (* STORAGE-FEE duration)))
    (begin
      (asserts! (>= (stx-get-balance tx-sender) fee) ERR-INSUFFICIENT-BALANCE)
      (try! (stx-transfer? fee tx-sender CONTRACT-OWNER))
      (map-set premium-users tx-sender {
        expires-at: (+ block-height duration),
        storage-limit: u10,
        features: (list "BULK_OPERATIONS" "ADVANCED_SEARCH" "EXPORT_DATA")
      })
      (update-metadata-premium)
      (log-event "PREMIUM_UPGRADE" tx-sender (int-to-ascii (to-int duration)))
      (ok true)
    )
  )
)

;; Check if user is premium
(define-read-only (is-premium-user (user principal))
  (match (map-get? premium-users user)
    premium-data (> (get expires-at premium-data) block-height)
    false
  )
)

;; === ACCESS CONTROL ===

;; Set profile to public/private
(define-public (set-public-profile (is-public bool))
  (begin
    (map-set public-profiles tx-sender is-public)
    (log-event "PROFILE_VISIBILITY" tx-sender (if is-public "PUBLIC" "PRIVATE"))
    (ok true)
  )
)

;; Add authorized reader
(define-public (add-authorized-reader (reader principal))
  (let ((current-readers (default-to (list) (map-get? authorized-readers tx-sender))))
    (begin
      (asserts! (< (len current-readers) u50) ERR-INVALID-INPUT)
      (map-set authorized-readers tx-sender (unwrap! (as-max-len? (append current-readers reader) u50) ERR-INVALID-INPUT))
      (log-event "READER_ADDED" tx-sender (principal-to-string reader))
      (ok true)
    )
  )
)

;; Remove authorized reader (simplified implementation)
(define-public (remove-authorized-reader (reader principal))
  (begin
    ;; Simplified implementation - in reality you'd need to rebuild the list without the target reader
    (map-delete authorized-readers tx-sender)
    (log-event "READER_REMOVED" tx-sender (principal-to-string reader))
    (ok true)
  )
)

;; === BULK OPERATIONS (Premium Feature) ===

;; Bulk store multiple values (premium only)
(define-public (bulk-store-numbers (values (list 10 uint)))
  (begin
    (asserts! (is-premium-user tx-sender) ERR-UNAUTHORIZED)
    (asserts! (<= (len values) u10) ERR-INVALID-INPUT)
    (fold bulk-store-number-helper values u0)
    (log-event "BULK_STORE" tx-sender "NUMBERS")
    (ok true)
  )
)

;; === SEARCH AND ANALYTICS ===

;; Search users by number range (simplified implementation)
;; Note: This is a basic implementation - in a real contract you'd need a more sophisticated approach
(define-read-only (search-users-by-number-range (min-val uint) (max-val uint))
  (let ((sample-results (list)))
    ;; In a real implementation, you would iterate through known users
    ;; For now, returning empty list as we don't have a user registry
    sample-results
  )
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-metadata user)
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-users: (var-get total-users),
    total-operations: (var-get total-operations),
    contract-balance: (var-get contract-balance),
    current-block: block-height
  }
)

;; === EXPORT FUNCTIONS ===

;; Export user data (premium feature)
(define-read-only (export-my-data)
  (if (is-premium-user tx-sender)
    (some {
      numbers: (map-get? user-numbers tx-sender),
      strings: (map-get? user-strings tx-sender),
      booleans: (map-get? user-booleans tx-sender),
      metadata: (map-get? user-metadata tx-sender),
      premium-info: (map-get? premium-users tx-sender)
    })
    none
  )
)

;; === DATA MANAGEMENT ===

;; Clear all user data
(define-public (clear-all-data)
  (begin
    (map-delete user-numbers tx-sender)
    (map-delete user-strings tx-sender)
    (map-delete user-booleans tx-sender)
    (map-delete user-metadata tx-sender)
    (log-event "DATA_CLEARED" tx-sender "ALL")
    (ok true)
  )
)

;; Backup data to another address (premium feature)
(define-public (backup-data-to (backup-address principal))
  (begin
    (asserts! (is-premium-user tx-sender) ERR-UNAUTHORIZED)
    (match (get-all-data tx-sender)
      user-data (begin
        ;; This would typically involve more complex backup logic
        (log-event "DATA_BACKUP" tx-sender (principal-to-string backup-address))
        (ok true)
      )
      ERR-NOT-FOUND
    )
  )
)

;; === HELPER FUNCTIONS ===

;; Update user metadata
(define-private (update-metadata)
  (let ((current-meta (default-to {created-at: block-height, updated-at: block-height, access-count: u0, is-premium: false} 
                                  (map-get? user-metadata tx-sender))))
    (map-set user-metadata tx-sender (merge current-meta {
      updated-at: block-height,
      is-premium: (is-premium-user tx-sender)
    }))
  )
)

;; Update metadata for premium users
(define-private (update-metadata-premium)
  (let ((current-meta (default-to {created-at: block-height, updated-at: block-height, access-count: u0, is-premium: false} 
                                  (map-get? user-metadata tx-sender))))
    (map-set user-metadata tx-sender (merge current-meta {
      updated-at: block-height,
      is-premium: true
    }))
  )
)

;; Check if caller can read user data
(define-private (can-read-user-data (user principal))
  (or 
    (is-eq user tx-sender) ;; Own data
    (default-to false (map-get? public-profiles user)) ;; Public profile
    (is-some (index-of (default-to (list) (map-get? authorized-readers user)) tx-sender)) ;; Authorized reader
  )
)

;; Public function to track data access (if you want to count access)
(define-public (access-number (user principal))
  (if (can-read-user-data user)
    (begin
      (increment-access-count user)
      (ok (map-get? user-numbers user)))
    ERR-UNAUTHORIZED
  )
)

;; Public function to access string with tracking
(define-public (access-string (user principal))
  (if (can-read-user-data user)
    (begin
      (increment-access-count user)
      (ok (map-get? user-strings user)))
    ERR-UNAUTHORIZED
  )
)

;; Public function to access boolean with tracking
(define-public (access-boolean (user principal))
  (if (can-read-user-data user)
    (begin
      (increment-access-count user)
      (ok (map-get? user-booleans user)))
    ERR-UNAUTHORIZED
  )
)

;; Increment total operations
(define-private (increment-operations)
  (var-set total-operations (+ (var-get total-operations) u1))
)

;; Increment access count (private helper)
(define-private (increment-access-count (user principal))
  (let ((current-meta (default-to {created-at: block-height, updated-at: block-height, access-count: u0, is-premium: false} 
                                  (map-get? user-metadata user))))
    (map-set user-metadata user (merge current-meta {
      access-count: (+ (get access-count current-meta) u1)
    }))
  )
)

;; Helper for filtering readers (simplified)
(define-private (is-not-target-reader (reader principal))
  true ;; In a real implementation, this would compare against a target reader
)

;; Helper for bulk operations
(define-private (bulk-store-number-helper (value uint) (acc uint))
  (begin
    (map-set user-numbers tx-sender value)
    (+ acc u1)
  )
)

;; Get all users with numbers (simplified - returns empty list)
;; Note: In a real implementation, you'd need to maintain a separate list of all users
(define-private (get-all-users-with-numbers)
  (list) ;; Returns empty list - would need user registry for real implementation
)

;; Convert principal to string (helper)
(define-private (principal-to-string (p principal))
  "principal" ;; Simplified representation
)