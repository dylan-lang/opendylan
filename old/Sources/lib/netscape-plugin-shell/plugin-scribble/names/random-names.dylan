module: random-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("random");

module-namer("random");

constant-namer("_L_random_distribution_G_", <random-distribution>);

constant-namer("random", random);

constant-namer("_L_uniform_distribution_G_", <uniform-distribution>);

constant-namer("_L_unit_uniform_distribution_G_", <unit-uniform-distribution>);

constant-namer("_L_real_uniform_distribution_G_", <real-uniform-distribution>);

constant-namer("_L_integer_uniform_distribution_G_", <integer-uniform-distribution>);

constant-namer("_L_exponential_distribution_G_", <exponential-distribution>);

constant-namer("_L_normal_distribution_G_", <normal-distribution>);

variable-namer("_T_dylan_random_seed_T_", *dylan-random-seed*);

variable-namer("_T_dylan_random_distribution_T_", *dylan-random-distribution*);

constant-namer("random_uniform", random-uniform);

constant-namer("seed_random_E_", seed-random!);

constant-namer("chi_square", chi-square);
