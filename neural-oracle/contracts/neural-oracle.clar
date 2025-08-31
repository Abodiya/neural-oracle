;; Neural Oracle Network Smart Contract

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

;; Contract Owner
(define-data-var contract-owner principal tx-sender)
(define-data-var network-fee-rate uint u250) ;; 2.5%
(define-data-var min-accuracy-score uint u50)
(define-data-var oracle-pool-fee uint u100) ;; 1%

;; Oracle Node Profile Data
(define-map oracle-nodes 
    { node: principal }
    {
        total-predictions: uint,
        accuracy-score: uint,
        reliability-score: uint,
        token-supply: uint,
        revenue-earned: uint,
        data-feed-count: uint,
        subscriber-count: uint,
        reputation: uint
    }
)

;; Dynamic Node Token Values
(define-map node-token-prices
    { node: principal }
    {
        current-price: uint,
        last-update: uint,
        price-trend: int,
        volume-24h: uint
    }
)

;; Subscription NFT Tiers
(define-map subscription-nfts
    { node: principal, subscriber: principal }
    {
        tier-level: uint, ;; 1-basic, 2-standard, 3-premium, 4-enterprise
        accuracy-score: uint,
        total-contributions: uint,
        tier-benefits: uint,
        revenue-share-rate: uint,
        mint-timestamp: uint
    }
)

;; Neural Model Validation Data
(define-map model-validations
    { validator: principal, data-feed-id: uint }
    {
        model-score: uint,
        accuracy-score: uint,
        prediction-quality: uint,
        timestamp: uint,
        verified: bool,
        reward-earned: uint
    }
)

;; Data Feed Registry for IP Protection
(define-map data-feed-registry
    { data-feed-id: uint }
    {
        node: principal,
        data-hash: (buff 32),
        timestamp: uint,
        ip-protected: bool,
        prediction-count: uint,
        revenue-generated: uint
    }
)

;; Predictive Analytics Campaigns
(define-map analytics-campaigns
    { campaign-id: uint }
    {
        node: principal,
        target-metric: uint,
        prediction-pool: uint,
        end-timestamp: uint,
        actual-result: uint,
        settled: bool,
        total-stakes: uint
    }
)

;; Subscriber Predictions
(define-map subscriber-predictions
    { campaign-id: uint, subscriber: principal }
    {
        predicted-value: uint,
        stake-amount: uint,
        potential-reward: uint,
        claimed: bool
    }
)

;; Multi-Modal Data Credits
(define-map data-credits
    { user: principal }
    {
        api-credits: uint,
        social-credits: uint,
        iot-credits: uint,
        financial-credits: uint,
        total-credits: uint,
        conversion-rate: uint
    }
)

;; Revenue Distribution Pools
(define-map node-revenue-pools
    { node: principal }
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
    (if (is-eq tier u1) u10  ;; basic: 10% benefits
    (if (is-eq tier u2) u25  ;; standard: 25% benefits
    (if (is-eq tier u3) u50  ;; premium: 50% benefits
        u100)))              ;; enterprise: 100% benefits
)

(define-private (calculate-revenue-share (tier uint))
    (if (is-eq tier u1) u5   ;; basic: 5% revenue share
    (if (is-eq tier u2) u10  ;; standard: 10% revenue share
    (if (is-eq tier u3) u20  ;; premium: 20% revenue share
        u30)))               ;; enterprise: 30% revenue share
)

(define-private (calculate-accuracy-score (model uint) (prediction uint))
    (let ((base-score (/ (+ model prediction) u2)))
        (if (> base-score u90) u95
        (if (> base-score u70) u80
        (if (> base-score u50) u65
            u45)))
    )
)

(define-private (calculate-prediction-quality (model uint) (prediction uint))
    (let ((quality-base (/ (+ (* model u3) prediction) u4)))
        (if (> quality-base u80) u90
        (if (> quality-base u60) u75
            u50))
    )
)

(define-private (calculate-validation-reward (quality-score uint))
    (if (> quality-score u80) u1000000  ;; 1 STX for high quality
    (if (> quality-score u60) u500000   ;; 0.5 STX for medium quality
        u250000))                       ;; 0.25 STX for basic quality
)

(define-private (calculate-analytics-reward (predicted-value uint) (stake-amount uint))
    (let ((multiplier (if (> predicted-value u1000000) u150 u120))) ;; 1.5x or 1.2x multiplier
        (/ (* stake-amount multiplier) u100)
    )
)

;; Admin Functions
(define-public (set-network-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-fee u1000) ERR-INVALID-AMOUNT)
        (ok (var-set network-fee-rate new-fee))
    )
)

(define-public (update-min-accuracy-score (new-score uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-score u100) ERR-INVALID-ACCURACY-SCORE)
        (ok (var-set min-accuracy-score new-score))
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)

;; Oracle Node Registration and Management
(define-public (register-oracle-node)
    (let ((node tx-sender))
        (asserts! (is-none (map-get? oracle-nodes { node: node })) ERR-ALREADY-EXISTS)
        (map-set oracle-nodes
            { node: node }
            {
                total-predictions: u0,
                accuracy-score: u50,
                reliability-score: u50,
                token-supply: u1000000,
                revenue-earned: u0,
                data-feed-count: u0,
                subscriber-count: u0,
                reputation: u50
            }
        )
        (map-set node-token-prices
            { node: node }
            {
                current-price: u1000000, ;; 1 STX in microSTX
                last-update: block-height,
                price-trend: 0,
                volume-24h: u0
            }
        )
        (map-set node-revenue-pools
            { node: node }
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

(define-public (mint-subscription-nft (node principal) (tier uint))
    (let 
        ((subscriber tx-sender)
         (existing-nft (map-get? subscription-nfts { node: node, subscriber: subscriber })))
        (asserts! (and (>= tier u1) (<= tier u4)) ERR-INVALID-TIER)
        (asserts! (is-some (map-get? oracle-nodes { node: node })) ERR-NODE-NOT-FOUND)
        
        (map-set subscription-nfts
            { node: node, subscriber: subscriber }
            {
                tier-level: tier,
                accuracy-score: u0,
                total-contributions: u0,
                tier-benefits: (calculate-tier-benefits tier),
                revenue-share-rate: (calculate-revenue-share tier),
                mint-timestamp: block-height
            }
        )
        
        ;; Update node subscriber count
        (match (map-get? oracle-nodes { node: node })
            node-data
            (map-set oracle-nodes
                { node: node }
                (merge node-data { subscriber-count: (+ (get subscriber-count node-data) u1) })
            )
            false
        )
        (ok true)
    )
)

(define-public (submit-model-validation (data-feed-id uint) (model-score uint) (prediction-data uint))
    (let ((validator tx-sender))
        (asserts! (and (>= model-score u1) (<= model-score u100)) ERR-INVALID-MODEL-SCORE)
        (asserts! (>= model-score (var-get min-accuracy-score)) ERR-INVALID-ACCURACY-SCORE)
        
        (let ((accuracy-score (calculate-accuracy-score model-score prediction-data))
              (quality-score (calculate-prediction-quality model-score prediction-data)))
            
            (map-set model-validations
                { validator: validator, data-feed-id: data-feed-id }
                {
                    model-score: model-score,
                    accuracy-score: accuracy-score,
                    prediction-quality: quality-score,
                    timestamp: block-height,
                    verified: (>= accuracy-score u70),
                    reward-earned: (calculate-validation-reward quality-score)
                }
            )
            
            ;; Update data feed prediction count if feed exists
            (match (map-get? data-feed-registry { data-feed-id: data-feed-id })
                feed-data
                (map-set data-feed-registry
                    { data-feed-id: data-feed-id }
                    (merge feed-data { prediction-count: (+ (get prediction-count feed-data) u1) })
                )
                false
            )
            (ok accuracy-score)
        )
    )
)

(define-public (register-data-feed (data-hash (buff 32)))
    (let 
        ((node tx-sender)
         (data-feed-id (var-get next-data-feed-id)))
        
        (asserts! (is-some (map-get? oracle-nodes { node: node })) ERR-NODE-NOT-FOUND)
        
        (map-set data-feed-registry
            { data-feed-id: data-feed-id }
            {
                node: node,
                data-hash: data-hash,
                timestamp: block-height,
                ip-protected: true,
                prediction-count: u0,
                revenue-generated: u0
            }
        )
        
        (var-set next-data-feed-id (+ data-feed-id u1))
        
        ;; Update node data feed count
        (match (map-get? oracle-nodes { node: node })
            node-data
            (map-set oracle-nodes
                { node: node }
                (merge node-data { data-feed-count: (+ (get data-feed-count node-data) u1) })
            )
            false
        )
        (ok data-feed-id)
    )
)

(define-public (create-analytics-campaign (target-metric uint) (duration uint))
    (let 
        ((node tx-sender)
         (campaign-id (var-get next-campaign-id))
         (end-time (+ block-height duration)))
        
        (asserts! (is-some (map-get? oracle-nodes { node: node })) ERR-NODE-NOT-FOUND)
        (asserts! (> target-metric u0) ERR-INVALID-AMOUNT)
        (asserts! (> duration u0) ERR-INVALID-TIMESTAMP)
        
        (map-set analytics-campaigns
            { campaign-id: campaign-id }
            {
                node: node,
                target-metric: target-metric,
                prediction-pool: u0,
                end-timestamp: end-time,
                actual-result: u0,
                settled: false,
                total-stakes: u0
            }
        )
        
        (var-set next-campaign-id (+ campaign-id u1))
        (ok campaign-id)
    )
)

(define-public (stake-on-analytics (campaign-id uint) (predicted-value uint) (stake-amount uint))
    (let ((subscriber tx-sender))
        (asserts! (> stake-amount u0) ERR-INVALID-AMOUNT)
        
        (match (map-get? analytics-campaigns { campaign-id: campaign-id })
            campaign-data
            (begin
                (asserts! (< block-height (get end-timestamp campaign-data)) ERR-ORACLE-CLOSED)
                
                (map-set subscriber-predictions
                    { campaign-id: campaign-id, subscriber: subscriber }
                    {
                        predicted-value: predicted-value,
                        stake-amount: stake-amount,
                        potential-reward: (calculate-analytics-reward predicted-value stake-amount),
                        claimed: false
                    }
                )
                
                (map-set analytics-campaigns
                    { campaign-id: campaign-id }
                    (merge campaign-data 
                        { 
                            prediction-pool: (+ (get prediction-pool campaign-data) stake-amount),
                            total-stakes: (+ (get total-stakes campaign-data) u1)
                        }
                    )
                )
                (ok true)
            )
            ERR-DATA-FEED-NOT-FOUND
        )
    )
)

(define-public (settle-analytics-campaign (campaign-id uint) (actual-result uint))
    (let ((settler tx-sender))
        (match (map-get? analytics-campaigns { campaign-id: campaign-id })
            campaign-data
            (begin
                (asserts! (is-eq settler (get node campaign-data)) ERR-NOT-AUTHORIZED)