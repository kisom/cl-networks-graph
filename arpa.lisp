
(defvar SRI  (node 'sri  '(utah stan ucla ucsb)))
(defvar UCSB (node 'ucsb '(sri ucla)))
(defvar UCLA (node 'ucla '(ucsb stan sri rand)))
(defvar STAN (node 'stan '(sri ucla)))
(defvar UTAH (node 'utah '(sri sdc mit)))
(defvar SDC (node 'sdc '(utah rand)))
(defvar RAND (node 'rand '(ucla bbn sdc)))
(defvar MIT (node 'mit '(utah bbn linc)))
(defvar BBN (node 'bbn '(rand mit harv)))
(defvar HARV (node 'harv '(bbn carn)))
(defvar CARN (node 'carn '(harv case)))
(defvar CASE2 (node 'case '(carn linc)))
(defvar LINC (node 'linc '(case mit)))
(defvar ARPA-NET (list SRI
                        UCSB
                        UCLA
                        STAN
                        UTAH
                        SDC
                        RAND
                        MIT
                        BBN
                        HARV
                        CARN
                        CASE2
                        LINC))
