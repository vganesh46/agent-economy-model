# agent-economy-model

Mobility:

1000 agents are positioned initially within a boundary 1 x 1
1000 objects are distributed within a boundary 10 x 10.

Each go,
Agent moves a random distance d between -1 & 1.
Agent's wealth is incremented by 1 unit if it is within a distance of 0.01 to any of the object.
Agent identifies 2 closest other agents and moves half the distance to their average position (ft).
