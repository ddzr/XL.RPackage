library(usethis)

use_r('internal_helper_function')

use_r('ml_mean_target_encoder')

use_r('ml_mean_target_apply')

#update documents when finishing update function documentation
devtools::document()
#install package, check how it goes
devtools::install()
