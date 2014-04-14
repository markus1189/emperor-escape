### Escape - [Source](https://icpcarchive.ecs.baylor.edu/index.php?option=com_onlinejudge&Itemid=8&category=610&page=show_problem&problem=4595) ###

After an epic fight with the emperor lich, the hero struggles to
escape the dungeon consisting of `n` chambers and `n−1` corridors
connecting them.

He starts in chamber number `1` and must reach chamber number `t`,
moving only along the corridors.  All chambers are reachable from
chamber number `1`.

Bruised after the last fight, the hero starts the journey with `0`
hit-points (HP).  These points represent his health - if ever they
fall below zero, the hero’s story ends there as a tragic one.

In some chambers there are monsters - a monster must be fought, and it
always manages to take some of the hero’s HP.  In some other chambers
there are magic pools - every pool restores some number of the
hit-points.

There is no upper limit on the hero’s health.  Every chamber can be
visited multiple times, but the gain or loss of HP happens only once,
on the very first visit.

Determine whether the hero can escape the dungeon alive.

#### Input ####

The first line of input contains the number of test cases `T`.

The descriptions of the test cases follow:

The first line of each test case contains two integers: the number of
chambers `n`, `2 <= n <= 200000`, and the number of the exit chamber
`t`, `2 <= t <= n`.

The second line contains `n` space separated integers between `−10^6`
and `10^6` - the i-th of them denotes the HP gain in the i-th chamber
(negative denotes a monster, positive - a pool, and zero means that
the chamber is empty).  The first chamber does not contain a monster,
but a pool is possible there.  The exit chamber may contain a pool or
a monster, and the monster will have to be fought before escaping.

The next `n−1` lines contain the descriptions of corridors.  Each one
contains a pair of integers - the ends of a corridor.

#### Output ####

For each test case print a single line containing the word ‘escaped’
if escape is possible, or ‘trapped’ otherwise.

Sample Input
```
2
7 7
0 -3 2 2 3 -4 0
1 2
2 3
2 4
1 5
5 6
6 7
3 2
3 3 -4
1 3
2 3
```

Sample Output
```
escaped
trapped
```
