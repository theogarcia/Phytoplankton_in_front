// 


data {
    int<lower=1> N;                   // Nombre d'observations dans F
    int<lower=1> C;                   // Nombre de PFG
    int<lower=1> K;                   // Nombre de composants dans le mélange final (fixé à 3 ici)
    matrix[N, C] y;              // Observations (tableau N x C)
    
    
    
    /// Pour Composante A
    int<lower=1> K_A;                   // Nombre de composantes dans A 
    // SI K_A > 1
    //simplex[K_A] alpha_A;               // Vecteurs estimés des poids des K_A composantes du mélange
    matrix[K_A, C] mu_A;              // Moyennes estimées des K_A composantes dans A
    real sigma_A[C, C ,K_A];  // Liste contenant les K_A matrices de covariance estimées dans A
    
    
    /// Pour Composante B
    int<lower=1> K_B;                   // Nombre de composantes dans B    
    // SI K_B > 1
    simplex[K_B] alpha_B;               // Vecteurs estimés des poids des K_A composantes du mélange
    matrix[K_B, C] mu_B;              // Moyennes estimées des K_B composantes dans B
    real sigma_B[C, C, K_B];  // Liste contenant les K_B matrices de covariance estimées dans B
    
    
    /// Pour composante C
    int<lower=1> K_C;                   // Nombre de composantes dans C    
    matrix[K_C, C] mu_C;              // Moyennes estimées des K_C composantes dans C
    real sigma_C [C, C, K_C];  // Liste contenant les K_C matrices de covariance estimées dans C

}

parameters {
    //Si K_A = 1 on laisse la ligne suivante
    simplex[K_A] alpha_A;               // poids des K_C composantes du mélange dans C
    //Si K_B = 1 on laisse la ligne suivante
    //simplex[K_B] alpha_B;               // poids des K_C composantes du mélange dans C
    simplex[K_C] alpha_C;               // poids des K_C composantes du mélange dans C
    simplex[K] lambda;                // poids des K composantes du mélange final
}

model {
    // Priors
    lambda ~ dirichlet(rep_vector(1.0, K));  // Prior sur les poids
    alpha_C ~ dirichlet(rep_vector(1.0, K_C));  // Prior sur les poids
    alpha_B ~ dirichlet(rep_vector(1.0, K_B));  // Prior sur les poids
    
    for(n in 1:N){
        vector[K_A] mix_A;
        for (k in 1:K_A) {
            matrix[C, C] sigma_A_k;
            for (i in 1:C) {
                for (j in 1:C) {
                    sigma_A_k[i, j] = sigma_A[i, j, k];
                }
            }
            mix_A[k] = log(alpha_A[k]) + multi_normal_lpdf(to_vector(y[n]) | to_vector(mu_A[k]), sigma_A_k);
        }
        real log_sum_A = log_sum_exp(mix_A);
        
        vector[K_B] mix_B;
        for (k in 1:K_B) {
            matrix[C, C] sigma_B_k;
            for (i in 1:C) {
                for (j in 1:C) {
                    sigma_B_k[i, j] = sigma_B[i, j, k];
                }
            }
            mix_B[k] = log(alpha_B[k]) + multi_normal_lpdf(to_vector(y[n]) | to_vector(mu_B[k]), sigma_B_k);
        }
        real log_sum_B = log_sum_exp(mix_B);
        
        vector[K_C] mix_C;
        for (k in 1:K_C) {
            matrix[C, C] sigma_C_k;
            for (i in 1:C) {
                for (j in 1:C) {
                    sigma_C_k[i, j] = sigma_C[i, j, k];
                }
            }
            mix_C[k] = log(alpha_C[k]) + multi_normal_lpdf(to_vector(y[n]) | to_vector(mu_C[k]), sigma_C_k);
        }
        real log_sum_C = log_sum_exp(mix_C);

        vector[K] mix_lik;
        for (k in 1:K) {
            if(k == 1) {
                mix_lik[k] = log(lambda[k]) + log_sum_A;
            } else if(k == 2) {
                mix_lik[k] = log(lambda[k]) + log_sum_B;
            } else if(k == 3) {
                mix_lik[k] = log(lambda[k]) + log_sum_C;
            }
        }

        target += log_sum_exp(mix_lik);
    }
}


//generated quantities {
//  real log_lik[N];  // Log-vraisemblance pour chaque observation
      
//  for (n in 1:N) {
//    vector[K] mix_lik; // Log-vraisemblance pour chaque composante
//    for (k in 1:K) {
//      mix_lik[k] = log(lambda[k]) + multi_normal_lpdf(y[n] | mu[k],  matrices[k]);
//    }
//    log_lik[n] = log_sum_exp(mix_lik); // Combinaison dans l'espace log
//  }
//}


