#if defined(SUPERLU_MT)
# define sparse_solver_init solver_init_SuperLU_MT
# define sparse_solver_aux solver_SuperLU_MT
# define sparse_solver_cleanup solver_cleanup_SuperLU_MT

#elif defined(SUPERLU)
# define sparse_solver_init solver_init_SuperLU
# define sparse_solver_aux solver_SuperLU
# define sparse_solver_cleanup solver_cleanup_SuperLU

#elif defined(VIENNA_CL)
# define sparse_solver_init solver_init_ViennaCL
# define sparse_solver_aux solver_ViennaCL
# define sparse_solver_cleanup solver_cleanup_ViennaCL

#elif defined(MUMPS)
# define sparse_solver_init solver_init_MUMPS
# define sparse_solver_aux solver_MUMPS
# define sparse_solver_cleanup solver_cleanup_MUMPS

#else
#error No SPARSE_SOLVER configured!
#endif
