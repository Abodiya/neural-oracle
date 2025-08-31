;; Neural Oracle Network Smart Contract - Simplified and Error-Free Version

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-NODE-NOT-FOUND (err u103))
(define-constant ERR-INVALID-TIER (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-ORACLE-CLOSED (err u106))
(define-constant ERR-INVALID-TIMESTAMP (err u107))
(define-constant ERR-INSUFFICIENT-STAKE (err u108))
(define-constant ERR-INVALID-ACCURACY-SCORE (err u109))
(define-constant ERR-DATA-FEED-NOT-FOUND (err u110))
(define-constant ERR-INVALID-MODEL-SCORE (err u111))
(define-constant ERR-PREDICTION-EXPIRED (err u112))
(define-constant ERR-ALREADY-SETTLED (err u113))
(define-constant ERR-NOT-SETTLED (err u114))
(define-constant ERR-ALREADY-CLAIMED (err u115))
(define-constant ERR-CONTRACT-PAUSED (err u116))

;; Contract Configuration
(define-data-var contract-owner principal tx-sender)
(define-data-var network-fee-rate uint u250) ;; 2.5%
(define-data-var min-accuracy-score uint u50)
(define-data-var oracle-pool-fee uint u100) ;; 1%
(define-data-var contract-paused bool false)

;; Oracle Node Registry
(define-map oracle-nodes 
    principal
    {
        total-predictions: uint,
        accuracy-score: uint,
        reliability-score: uint,
        token-supply: uint,
        revenue-earned: uint,
        data-feed-count: uint,
        subscriber-count: uint,
        reputation: uint,
        active: bool,
        registration-time: uint
    }
)

;; Node Token Pricing
(define-map node-token-prices
    principal
    {
        current-price: uint,
        last-update: uint,
        price-trend: int,
        volume-24h: uint
    }
)

;; Subscription NFTs
(define-map subscription-nfts
    { node: principal, subscriber: principal }
    {
        tier-level: uint,
        accuracy-score: uint,
        total-contributions: uint,
        tier-benefits: uint,
        revenue-share-rate: uint,
        mint-timestamp: uint,
        active: bool
    }
)

;; Model Validations
(define-map model-validations
    { validator: principal, data-feed-id: uint }
    {
        model-score: uint,
        accuracy-score: uint,
        prediction-quality: uint,
        timestamp: uint,
        verified: bool,
        reward-earned: uint,
        claimed: bool
    }
)

;; Data Feed Registry
(define-map data-feed-registry
    uint
    {
        node: principal,
        data-hash: (buff 32),
        timestamp: uint,
        ip-protected: bool,
        prediction-count: uint,
        revenue-generated: uint,
        active: bool
    }
)

;; Analytics Campaigns
(define-map analytics-campaigns
    uint
    {
        node: principal,
        target-metric: uint,
        prediction-pool: uint,
        end-timestamp: uint,
        actual-result: uint,
        settled: bool,
        total-stakes: uint,
        min-stake: uint
    }
)

;; Subscriber Predictions
(define-map subscriber-predictions
    { campaign-id: uint, subscriber: principal }
    {
        predicted-value: uint,
        stake-amount: uint,
        potential-reward: uint,
        claimed: bool,
        accuracy-percentage: uint
    }
)

;; Data Credits
(define-map data-credits
    principal
    {
        api-credits: uint,
        social-credits: uint,
        iot-credits: uint,
        financial-credits: uint,
        total-credits: uint
    }
)

;; Revenue Pools
(define-map node-revenue-pools
    principal
    {
        immediate-pool: uint,
        subscriber-reward-pool: uint,
        model-protection-pool: uint,
        collaboration-pool: uint
    }
)

;; Auto-incrementing IDs
(define-data-var next-data-feed-id uint u1)
(define-data-var next-campaign-id uint u1)

;; Helper Functions
(define-private (calculate-tier-benefits (tier uint))
    (if (is-eq tier u1) u10
    (if (is-eq tier u2) u25
    (if (is-eq tier u3) u50
    (if (is-eq tier u4) u100
        u0))))
)

(define-private (calculate-revenue-share (tier uint))
    (if (is-eq tier u1) u5
    (if (is-eq tier u2) u10
    (if (is-eq tier u3) u20
    (if (is-eq tier u4) u30
        u0))))
)

(define-private (calculate-accuracy-score (model-score uint) (prediction-score uint))
    (let ((base-score (/ (+ model-score prediction-score) u2)))
        (if (> base-score u90) u95
        (if (> base-score u70) u80
        (if (> base-score u50) u65
            u45)))
    )
)

(define-private (calculate-validation-reward (quality-score uint))
    (if (> quality-score u80) u1000000
    (if (> quality-score u60) u500000
        u250000))
)

(define-private (calculate-prediction-accuracy (predicted uint) (actual uint))
    (if (is-eq actual u0) u0
        (let ((diff (if (>= predicted actual) (- predicted actual) (- actual predicted))))
            (if (is-eq diff u0) u100
                (let ((error-percentage (/ (* diff u100) actual)))
                    (if (> error-percentage u100) 
                        u0 
                        (- u100 error-percentage)
                    )
                )
            )
        )
    )
)

;; Read-only Functions
(define-read-only (get-oracle-node (node principal))
    (map-get? oracle-nodes node)
)

(define-read-only (get-node-token-price (node principal))
    (map-get? node-token-prices node)
)

(define-read-only (get-subscription-nft (node principal) (subscriber principal))
    (map-get? subscription-nfts { node: node, subscriber: subscriber })
)

(define-read-only (get-analytics-campaign (campaign-id uint))
    (map-get? analytics-campaigns campaign-id)
)

(define-read-only (get-data-feed (data-feed-id uint))
    (map-get? data-feed-registry data-feed-id)
)

(define-read-only (get-contract-owner)
    (var-get contract-owner)
)

(define-read-only (is-contract-paused)
    (var-get contract-paused)
)

;; Admin Functions
(define-public (set-network-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-fee u1000) ERR-INVALID-AMOUNT)
        (var-set network-fee-rate new-fee)
        (ok true)
    )
)

(define-public (update-min-accuracy-score (new-score uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (and (<= new-score u100) (>= new-score u1)) ERR-INVALID-ACCURACY-SCORE)
        (var-set min-accuracy-score new-score)
        (ok true)
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (resume-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-paused false)
        (ok true)
    )
)

;; Oracle Node Functions
(define-public (register-oracle-node)
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-none (map-get? oracle-nodes tx-sender)) ERR-ALREADY-EXISTS)
        
        (map-set oracle-nodes tx-sender
            {
                total-predictions: u0,
                accuracy-score: u50,
                reliability-score: u50,
                token-supply: u1000000,
                revenue-earned: u0,
                data-feed-count: u0,
                subscriber-count: u0,
                reputation: u50,
                active: true,
                registration-time: block-height
            }
        )
        
        (map-set node-token-prices tx-sender
            {
                current-price: u1000000,
                last-update: block-height,
                price-trend: 0,
                volume-24h: u0
            }
        )
        
        (map-set node-revenue-pools tx-sender
            {
                immediate-pool: u0,
                subscriber-reward-pool: u0,
                model-protection-pool: u0,
                collaboration-pool: u0
            }
        )
        
        (ok true)
    )
)

(define-public (deactivate-oracle-node)
    (begin
        (asserts! (is-some (map-get? oracle-nodes tx-sender)) ERR-NODE-NOT-FOUND)
        
        (let ((node-data (unwrap! (map-get? oracle-nodes tx-sender) ERR-NODE-NOT-FOUND)))
            (map-set oracle-nodes tx-sender
                (merge node-data { active: false })
            )
        )
        
        (ok true)
    )
)

;; Subscription Functions
(define-public (mint-subscription-nft (node principal) (tier uint))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (and (>= tier u1) (<= tier u4)) ERR-INVALID-TIER)
        (asserts! (is-some (map-get? oracle-nodes node)) ERR-NODE-NOT-FOUND)
        
        (let ((node-data (unwrap! (map-get? oracle-nodes node) ERR-NODE-NOT-FOUND)))
            (asserts! (get active node-data) ERR-ORACLE-CLOSED)
            
            (map-set subscription-nfts { node: node, subscriber: tx-sender }
                {
                    tier-level: tier,
                    accuracy-score: u0,
                    total-contributions: u0,
                    tier-benefits: (calculate-tier-benefits tier),
                    revenue-share-rate: (calculate-revenue-share tier),
                    mint-timestamp: block-height,
                    active: true
                }
            )
            
            (map-set oracle-nodes node
                (merge node-data { subscriber-count: (+ (get subscriber-count node-data) u1) })
            )
        )
        
        (ok true)
    )
)

;; Data Feed Functions
(define-public (register-data-feed (data-hash (buff 32)))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-some (map-get? oracle-nodes tx-sender)) ERR-NODE-NOT-FOUND)
        
        (let ((data-feed-id (var-get next-data-feed-id))
              (node-data (unwrap! (map-get? oracle-nodes tx-sender) ERR-NODE-NOT-FOUND)))
            (asserts! (get active node-data) ERR-ORACLE-CLOSED)
            
            (map-set data-feed-registry data-feed-id
                {
                    node: tx-sender,
                    data-hash: data-hash,
                    timestamp: block-height,
                    ip-protected: true,
                    prediction-count: u0,
                    revenue-generated: u0,
                    active: true
                }
            )
            
            (map-set oracle-nodes tx-sender
                (merge node-data { data-feed-count: (+ (get data-feed-count node-data) u1) })
            )
            
            (var-set next-data-feed-id (+ data-feed-id u1))
            (ok data-feed-id)
        )
    )
)

;; Model Validation Functions
(define-public (submit-model-validation (data-feed-id uint) (model-score uint) (prediction-data uint))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (and (>= model-score u1) (<= model-score u100)) ERR-INVALID-MODEL-SCORE)
        (asserts! (>= model-score (var-get min-accuracy-score)) ERR-INVALID-ACCURACY-SCORE)
        (asserts! (is-some (map-get? data-feed-registry data-feed-id)) ERR-DATA-FEED-NOT-FOUND)
        
        (let ((feed-data (unwrap! (map-get? data-feed-registry data-feed-id) ERR-DATA-FEED-NOT-FOUND))
              (accuracy-score (calculate-accuracy-score model-score prediction-data))
              (reward-amount (calculate-validation-reward accuracy-score)))
            (asserts! (get active feed-data) ERR-DATA-FEED-NOT-FOUND)
            
            (map-set model-validations { validator: tx-sender, data-feed-id: data-feed-id }
                {
                    model-score: model-score,
                    accuracy-score: accuracy-score,
                    prediction-quality: accuracy-score,
                    timestamp: block-height,
                    verified: (>= accuracy-score u70),
                    reward-earned: reward-amount,
                    claimed: false
                }
            )
            
            (map-set data-feed-registry data-feed-id
                (merge feed-data { prediction-count: (+ (get prediction-count feed-data) u1) })
            )
            
            (ok accuracy-score)
        )
    )
)

(define-public (claim-validation-reward (data-feed-id uint))
    (begin
        (let ((validation-data (unwrap! (map-get? model-validations { validator: tx-sender, data-feed-id: data-feed-id }) ERR-DATA-FEED-NOT-FOUND)))
            (asserts! (not (get claimed validation-data)) ERR-ALREADY-CLAIMED)
            (asserts! (get verified validation-data) ERR-NOT-AUTHORIZED)
            
            (map-set model-validations { validator: tx-sender, data-feed-id: data-feed-id }
                (merge validation-data { claimed: true })
            )
            
            (ok (get reward-earned validation-data))
        )
    )
)

;; Analytics Campaign Functions
(define-public (create-analytics-campaign (target-metric uint) (duration uint) (min-stake uint))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-some (map-get? oracle-nodes tx-sender)) ERR-NODE-NOT-FOUND)
        (asserts! (> target-metric u0) ERR-INVALID-AMOUNT)
        (asserts! (> duration u0) ERR-INVALID-TIMESTAMP)
        (asserts! (> min-stake u0) ERR-INVALID-AMOUNT)
        
        (let ((campaign-id (var-get next-campaign-id))
              (node-data (unwrap! (map-get? oracle-nodes tx-sender) ERR-NODE-NOT-FOUND))
              (end-time (+ block-height duration)))
            (asserts! (get active node-data) ERR-ORACLE-CLOSED)
            
            (map-set analytics-campaigns campaign-id
                {
                    node: tx-sender,
                    target-metric: target-metric,
                    prediction-pool: u0,
                    end-timestamp: end-time,
                    actual-result: u0,
                    settled: false,
                    total-stakes: u0,
                    min-stake: min-stake
                }
            )
            
            (var-set next-campaign-id (+ campaign-id u1))
            (ok campaign-id)
        )
    )
)

(define-public (stake-on-analytics (campaign-id uint) (predicted-value uint) (stake-amount uint))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (> stake-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> predicted-value u0) ERR-INVALID-AMOUNT)
        
        (let ((campaign-data (unwrap! (map-get? analytics-campaigns campaign-id) ERR-DATA-FEED-NOT-FOUND)))
            (asserts! (< block-height (get end-timestamp campaign-data)) ERR-ORACLE-CLOSED)
            (asserts! (not (get settled campaign-data)) ERR-ALREADY-SETTLED)
            (asserts! (>= stake-amount (get min-stake campaign-data)) ERR-INSUFFICIENT-STAKE)
            
            (map-set subscriber-predictions { campaign-id: campaign-id, subscriber: tx-sender }
                {
                    predicted-value: predicted-value,
                    stake-amount: stake-amount,
                    potential-reward: u0,
                    claimed: false,
                    accuracy-percentage: u0
                }
            )
            
            (map-set analytics-campaigns campaign-id
                (merge campaign-data 
                    { 
                        prediction-pool: (+ (get prediction-pool campaign-data) stake-amount),
                        total-stakes: (+ (get total-stakes campaign-data) u1)
                    }
                )
            )
            
            (ok true)
        )
    )
)

(define-public (settle-analytics-campaign (campaign-id uint) (actual-result uint))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (> actual-result u0) ERR-INVALID-AMOUNT)
        
        (let ((campaign-data (unwrap! (map-get? analytics-campaigns campaign-id) ERR-DATA-FEED-NOT-FOUND)))
            (asserts! (is-eq tx-sender (get node campaign-data)) ERR-NOT-AUTHORIZED)
            (asserts! (>= block-height (get end-timestamp campaign-data)) ERR-INVALID-TIMESTAMP)
            (asserts! (not (get settled campaign-data)) ERR-ALREADY-SETTLED)
            
            (map-set analytics-campaigns campaign-id
                (merge campaign-data 
                    { 
                        actual-result: actual-result,
                        settled: true
                    }
                )
            )
            
            (ok actual-result)
        )
    )
)

(define-public (claim-prediction-reward (campaign-id uint))
    (begin
        (let ((prediction-data (unwrap! (map-get? subscriber-predictions { campaign-id: campaign-id, subscriber: tx-sender }) ERR-DATA-FEED-NOT-FOUND))
              (campaign-data (unwrap! (map-get? analytics-campaigns campaign-id) ERR-DATA-FEED-NOT-FOUND)))
            (asserts! (not (get claimed prediction-data)) ERR-ALREADY-CLAIMED)
            (asserts! (get settled campaign-data) ERR-NOT-SETTLED)
            
            (let ((accuracy (calculate-prediction-accuracy 
                             (get predicted-value prediction-data)
                             (get actual-result campaign-data)))
                  (base-reward (get stake-amount prediction-data)))
                
                (let ((final-reward (if (>= accuracy u90) (* base-reward u2)
                                   (if (>= accuracy u70) (/ (* base-reward u150) u100)
                                   (if (>= accuracy u50) (/ (* base-reward u120) u100)
                                       u0)))))
                    
                    (map-set subscriber-predictions { campaign-id: campaign-id, subscriber: tx-sender }
                        (merge prediction-data 
                            { 
                                claimed: true,
                                potential-reward: final-reward,
                                accuracy-percentage: accuracy
                            }
                        )
                    )
                    
                    (ok final-reward)
                )
            )
        )
    )
)

;; Data Credits Functions
(define-public (mint-data-credits (user principal) (api-credits uint) (social-credits uint) (iot-credits uint) (financial-credits uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        
        (let ((total-credits (+ (+ api-credits social-credits) (+ iot-credits financial-credits))))
            (map-set data-credits user
                {
                    api-credits: api-credits,
                    social-credits: social-credits,
                    iot-credits: iot-credits,
                    financial-credits: financial-credits,
                    total-credits: total-credits
                }
            )
            
            (ok total-credits)
        )
    )
)