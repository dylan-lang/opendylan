	.global primitive_wrap_machine_word
	.global primitive_wrap_c_pointer
	.global primitive_exit_application
	.global Pstatic_root
	.global Pimmut_root
	.global Pambig_root
	.global Pexact_root
	.global module_hInstance
	.global _init_dylan_library
	.global primitive_ensure_valid_teb
	.global primitive_register_traced_roots
	.global primitive_deregister_traced_roots
	.global dylan_init_thread_local
	.global primitive_call_first_dylan_iep

	.section .data
	.align 4


Pstatic_root:
	.long 0
	.align 4
	.type Pstatic_root,@object
	.size Pstatic_root,. - Pstatic_root

Pimmut_root:
	.long 0
	.align 4
	.type Pimmut_root,@object
	.size Pimmut_root,. - Pimmut_root

Pambig_root:
	.long 0
	.align 4
	.type Pambig_root,@object
	.size Pambig_root,. - Pambig_root

Pexact_root:
	.long 0
	.align 4
	.type Pexact_root,@object
	.size Pexact_root,. - Pexact_root

module_hInstance:
	.long 0
	.align 4
	.type module_hInstance,@object
	.size module_hInstance,. - module_hInstance

_init_dylan_library:
	.long 0
	.align 4
	.type _init_dylan_library,@object
	.size _init_dylan_library,. - _init_dylan_library

_dylan_data_start:

_dylan_data_end:

_dylan_vars_start:

_dylan_vars_end:

_dylan_objs_start:

_dylan_objs_end:

_dylan_fixup_start:

_dylan_fixup_end:

_dylan_import_start:

_dylan_import_end:

	.global primitive_remove_optionals

	.section .text
	.align 4


	.global init_dylan_data
	.type init_dylan_data,@function
init_dylan_data:
	.short	31752,678,-28671,4,12321,65460,-16991,0,13249,76,15584
	.short	Pexact_root@ha
	.short	14567
	.short	Pexact_root@l
	.short	-27423,65532,15584
	.short	_dylan_vars_end@ha
	.short	14567
	.short	_dylan_vars_end@l
	.short	-27423,65532,15584
	.short	_dylan_vars_start@ha
	.short	14567
	.short	_dylan_vars_start@l
	.short	-27423,65532,15584
	.short	Pstatic_root@ha
	.short	14567
	.short	Pstatic_root@l
	.short	-27423,65532,15584
	.short	_dylan_objs_end@ha
	.short	14567
	.short	_dylan_objs_end@l
	.short	-27423,65532,15456
	.short	_dylan_data_start@ha
	.short	14435
	.short	_dylan_data_start@l
	.short	15488
	.short	_dylan_data_end@ha
	.short	14468
	.short	_dylan_data_end@l
	.short	15520
	.short	Pambig_root@ha
	.short	14501
	.short	Pambig_root@l
	.short	15552
	.short	_dylan_objs_start@ha
	.short	14534
	.short	_dylan_objs_start@l
	.short	15584
	.short	primitive_register_traced_roots@ha
	.short	14567
	.short	primitive_register_traced_roots@l
	.short	31976,934,20096,33,12350,65460,-18015,0,12321,76,-32767,4,31752,934,20096,32


	.global call_init_dylan
	.type call_init_dylan,@function
call_init_dylan:
	.short	31752,678,-28671,4,12321,65460,-16991,0,13249,76,15584
	.short	_init_dylan_library@ha
	.short	-32537
	.short	_init_dylan_library@l
	.short	31976,934,20096,33,12350,65460,-18015,0,12321,76,-32767,4,31752,934,20096,32


	.global dylan_initialize
	.type dylan_initialize,@function
dylan_initialize:
	.short	31752,678,-28671,4,12321,65460,-16991,0,13249,76,12321,65528,14560,0,-27423,65532
	.short	-28610,65452,12321,65512,15712
	.short	init_dylan_data@ha
	.short	14699
	.short	init_dylan_data@l
	.short	32104,934,20096,33,12321,24,15584
	.short	primitive_ensure_valid_teb@ha
	.short	14567
	.short	primitive_ensure_valid_teb@l
	.short	31976,934,20096,33,-28546,65456,-32482,65456,14560,65535,-28440,20,12321,65512,-32642,65452
	.short	15488
	.short	call_init_dylan@ha
	.short	14468
	.short	call_init_dylan@l
	.short	14496,0,14528,0,15712
	.short	dylan_init_thread_local@ha
	.short	14699
	.short	dylan_init_thread_local@l
	.short	32104,934,20096,33,12321,24,-32482,65456,14560,0,-28440,20,14432,0,12350,65460
	.short	-18015,0,12321,76,-32767,4,31752,934,20096,32


	.global dylan_main
	.type dylan_main,@function
dylan_main:
	.short	31752,678,-28671,4,12321,65460,-16991,0,13249,76,12321,65512,15712
	.short	dylan_initialize@ha
	.short	14699
	.short	dylan_initialize@l
	.short	32104,934,20096,33,12321,24,15456
	.short	Pambig_root@ha
	.short	14435
	.short	Pambig_root@l
	.short	15488
	.short	Pstatic_root@ha
	.short	14468
	.short	Pstatic_root@l
	.short	15520
	.short	Pexact_root@ha
	.short	14501
	.short	Pexact_root@l
	.short	15584
	.short	primitive_deregister_traced_roots@ha
	.short	14567
	.short	primitive_deregister_traced_roots@l
	.short	31976,934,20096,33,15584
	.short	primitive_exit_application@ha
	.short	14567
	.short	primitive_exit_application@l
	.short	31976,934,20096,33,12350,65460,-18015,0,12321,76,-32767,4,31752,934,20096,32


	.global primitive_runtime_module_handle
	.type primitive_runtime_module_handle,@function
primitive_runtime_module_handle:
	.short	32744,678,12321,65528,-16447,0,24638,0,15584
	.short	module_hInstance@ha
	.short	14567
	.short	module_hInstance@l
	.short	-32665,0,15584
	.short	primitive_wrap_machine_word@ha
	.short	14567
	.short	primitive_wrap_machine_word@l
	.short	31976,934,20096,33,25537,0,-17471,0,12321,8,32744,934,20096,32


	.global DylanSOEntry
	.type DylanSOEntry,@function
DylanSOEntry:
	.short	31752,678,-28671,4,12321,65528,-16447,0,24638,0,15712
	.short	dylan_initialize@ha
	.short	14699
	.short	dylan_initialize@l
	.short	32104,934,20096,33,14432,0,25537,0,-17471,0,12321,8,-32767,4,31752,934
	.short	20096,32


	.global DylanSOExit
	.type DylanSOExit,@function
DylanSOExit:
	.short	31752,678,-28671,4,12321,65460,-16991,0,13249,76,15456
	.short	Pambig_root@ha
	.short	14435
	.short	Pambig_root@l
	.short	15488
	.short	Pstatic_root@ha
	.short	14468
	.short	Pstatic_root@l
	.short	15520
	.short	Pexact_root@ha
	.short	14501
	.short	Pexact_root@l
	.short	15584
	.short	primitive_deregister_traced_roots@ha
	.short	14567
	.short	primitive_deregister_traced_roots@l
	.short	31976,934,20096,33,14432,0,12350,65460,-18015,0,12321,76,-32767,4,31752,934
	.short	20096,32
