# Cluster-and-stationary-states-of-opinions-dissemination
Project for Advanced Multivariate Statistics (DSE)
Paola Serra and Marzio De Corato


Among the different contexts in which the Ising model [1] can be adapted, one
application is the dynamics of a binary opinion [2]. In this work, we would
find the stationary states of such a system using a Markov chain Monte Carlo
approach (Metropolis-Hastings algorithm). As already exploited in the literature
for the Ising model, we will follow the following steps: first, we will
consider a two-dimensional matrix in which the up/down states are randomly
distributed. Such a matrix would model, in our case, two different opinions
about a fact. The interaction will be limited to the first neighbours. Then we
will change one/a a few states and check if the total energy (evaluated with
the Ising hamiltonian) is decreased or increased. If the energy is decreased, the
new state is accepted (further details can be found here [3]). The procedure
will be repeated until no new state is accepted for a certain number of tentative.
Different external fields, as well as different interaction values, will be
considered. The code by which this algorithm will be implemented will be R
(perhaps with a Shiny app).

[1] https://stanford.edu/ jeffjar/statmech/intro4.html
[2] LI, Lingbo, et al. Binary opinion dynamics on signed networks based on
Ising model. Physica A: Statistical Mechanics and its Applications, 2019,
525: 433-442.
[3] http://phd.fisica.unimi.it/assets/Comp Phys-esercizio3-1.pdf
