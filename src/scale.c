/*  Copyright (c) 2015-2016 Drew Schmidt
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdbool.h>
#include <math.h>

#include "coop.h"
#include "utils/safeomp.h"
#include "utils/scale.h"


// center and/or scale x in place
int coop_scale(const bool centerx, const bool scalex, const int m, const int n, double *restrict x, double *restrict colmeans, double *restrict colvars)
{
  if (m == 0 || n == 0)
    return COOP_OK;
  
  // Doing both at once, if needed, is more performant
  if (centerx && scalex)
  {
    double colmean;
    double colvar;
    #pragma omp parallel for shared(x) if (m*n > OMP_MIN_SIZE)
    for (int j=0; j<n; j++)
    {
      centerscalevec(j, m, x, &colmean, &colvar);
      
      colmeans[j] = colmean;
      colvars[j] = colvar;
    }
    
  }
  else if (centerx)
  {
    #pragma omp parallel for shared(x) if (m*n > OMP_MIN_SIZE)
    for (int j=0; j<n; j++)
      colmeans[j] = centervec(j, m, x);
  }
  else if (scalex) // RMSE
  {
    #pragma omp parallel for shared(x) if (m*n > OMP_MIN_SIZE)
    for (int j=0; j<n; j++)
      colvars[j] = scalevec(j, m, x);
  }
  
  return COOP_OK;
}
