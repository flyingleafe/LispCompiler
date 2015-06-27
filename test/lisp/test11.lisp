;;; Dummy memory management test
(memmgr_init)
(memmgr_alloc 4)
(memmgr_alloc 16)
(printInt 228)
(memmgr_list)
(memmgr_alloc 8)
(printInt 1488)
(memmgr_free)
