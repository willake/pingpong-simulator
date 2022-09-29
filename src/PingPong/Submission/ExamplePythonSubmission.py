import logging
from typing import Optional, Tuple
from PPData import *

# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
LOGGING_LEVEL = logging.ERROR

NUMLINKS     = 4

BAT_COLOR    = "#341235"

LINK_COLORS  = ["#a111b2"] * NUMLINKS
LINK_LENGTHS = [0.3]       * NUMLINKS

JOINT_COLORS = ["#777777"] * NUMLINKS
JOINT_ANGLES = [-0.7, 1.2, 1.0, -0.5]

#Exercise 1
def name() -> str:
    return "Example Python Player"

def make_arm() -> Arm:
    arm = Arm([], Bat(BAT_COLOR))

    for i in range(NUMLINKS):
        link = Link(LINK_COLORS[i], LINK_LENGTHS[i])
        joint = Joint(JOINT_COLORS[i], JOINT_ANGLES[i])
        arm.append(link, joint)

    return arm

#Exercise 2
def detectCollision(snap1: Snapshot, snap2: Snapshot) -> Optional[Second]:
    return None

#Exercise 3
def controlArm(time: Second, control: Control, arm: Arm) -> Arm:
    return arm

def evaluateArm(arm: Arm) -> List[Pnt]:
    lengths = [link.llen for link, _ in arm.comp]
    pnts = [Pnt(0,0)]
    for l in lengths:
        pnts.append(pnts[-1] + Pnt(0,l))
    pnts.append(pnts[-1] + Pnt(0,0.1)) 
    return pnts

def dance(time: Second, arm: Arm) -> Control:
    return [ -2 * math.sin (2.2 * time)
            , -2 * math.cos (2.3 * time)
            ,  2 * math.sin (2.4 * time)
            ,  2 * math.cos (2.5 * time)
            ]

#Exercise 4
def handleCollision(snap1: Snapshot, snap2: Snapshot, time: Second) -> Tuple[Pnt, Vec]:
    return (Pnt(0,0), Vec(0,0))

#Exercise 5
def inverse(arm: Arm, seg: Seg) -> List[Radian]:
    return [0.0] * NUMLINKS

#Exercise 6
def plan(current_time: Second, arm: Arm, time_bound: Second, 
            seg: Seg, velocity: Vec) -> Control:
    return [0.0] * NUMLINKS

#Exercise 7
def action(time: Second, item: Item, arm: Arm, ball: BallState) -> Control:
    return [ -10 * math.sin (2.2 * time)
           , -10 * math.cos (2.3 * time)
           ,  10 * math.sin (2.4 * time)
           ,  10 * math.cos (2.5 * time)]

