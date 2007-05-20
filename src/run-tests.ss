#!/usr/bin/env ikarus --r6rs-script

(import (ikarus)
        (tests reader)
        (tests bytevectors))

(test-reader)
(test-bytevectors)
(printf "Happy Happy Joy Joy\n")
