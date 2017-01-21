# agent-economy-model

Mobility:

1000 agents are positioned initially within a boundary 1 x 1
1000 objects are distributed within a boundary 10 x 10.

Each go,
Agent moves a random distance d between -1 & 1.
Agent's wealth is incremented by 1 unit if it is within a distance of 0.01 to any of the object.
Agent identifies 2 closest other agents and moves half the distance to their average position (ft).


Consumption:

For commodity A (essential commodity with degree of need = 1):

At any point in time, depending on the wealth of each of the 1000 agents and the marginal utility of commodity A (depends on degree of need for A, agents appetite to commodity A, quantity of A), A has a certain market price derived from the demand/supply schedule.

As time progresses, wealth of agents change because of change in positions and their proximity to objects. This changes market price of commodity A with time t.





For commodities B,C,D (non-essential commodities with degree of need < 1):

Willingness of an agent to pay for these is dependent on income (delta in wealth) minus cost of A which is then divided proportionally among the available non-essential commodities.
