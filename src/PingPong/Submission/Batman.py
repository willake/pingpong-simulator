import logging
from msilib import sequence
from shutil import move
import numpy as np1
from typing import Optional, Tuple
from PPData import *

# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
LOGGING_LEVEL = logging.ERROR

NUMLINKS = 4

BAT_COLOR = "#341235"

LINK_COLORS = ["#a111b2"] * NUMLINKS
LINK_LENGTHS = [0.3] * NUMLINKS

JOINT_COLORS = ["#777777"] * NUMLINKS
JOINT_ANGLES = [-0.7, 1.2, 1.0, -0.5]

# Exercise 1


def name() -> str:
    return "Batman"


def make_arm() -> Arm:
    arm = Arm([], Bat(BAT_COLOR))

    for i in range(NUMLINKS):
        link = Link(LINK_COLORS[i], LINK_LENGTHS[i])
        joint = Joint(JOINT_COLORS[i], JOINT_ANGLES[i])
        arm.append(link, joint)

    return arm

# Exercise 2
def detectCollision(snap1: Snapshot, snap2: Snapshot) -> Optional[Second]:
    ax= snap1.ball.x
    ay= snap1.ball.y
    bx= snap2.ball.x - snap1.ball.x
    by= snap2.ball.y - snap1.ball.y
    cx= snap1.segment.p.x
    cy= snap1.segment.p.y
    dx= snap2.segment.p.x - snap1.segment.p.x
    dy= snap2.segment.p.y - snap1.segment.p.y
    ex= snap1.segment.q.x
    ey= snap1.segment.q.y
    fx= snap2.segment.q.x - snap1.segment.q.x
    fy= snap2.segment.q.y - snap1.segment.q.y

    A= dx*fy-bx*fy-dx*by-dy*fx+by*fx+dy*bx
    B= dx*ey+cx*fy-bx*ey-ax*fy-dx*ay-cx*by-dy*ex-cy*fx+by*ex+ay*fx+dy*ax+cy*bx
    C= cx*ey-ax*ey-cx*ay-cy*ex+ay*ex+cy*ax

    if not A < 0 and not A > 0 :
        if not B < 0 and not B > 0:
            return None
        else:
            t = -C/B
            if not t < 0 and not t > 1:
                return Second((snap1.time+(snap2.time-snap1.time)*(t)))
            else:
                return None

    d=(B*B)-(4*A*C)
    t1=(-B+math.sqrt(d))/(2*A)
    t2=(-B-math.sqrt(d))/(2*A)

    if not t1<0 and not t1>1 :
        return Second((snap1.time+(snap2.time-snap1.time)*(t1)))
    elif not t2<0 and not t2>1 :
        return Second((snap1.time+(snap2.time-snap1.time)*(t2)))
    else :
        return None

def pntToD3(pnt: Pnt):
    return np.array([pnt.x, pnt.y, 0])

def vecToD3(vec: Vec):
    return np.array([vec.a, vec.b, 0])

# will return radians
def getSegmentRotation(segment1: Seg, segment2: Seg) -> float:
    vector1 = segment1.q - segment1.p
    vector2 = segment2.q - segment2.p
    unitVector1 = vector1 / np.linalg.norm(vector1)
    unitVector2 = vector2 / np.linalg.norm(vector2)
    return np.arccos(np.dot(unitVector1, unitVector2)) 

# Exercise 3


def controlArm(time: Second, control: Control, arm: Arm) -> Arm:
    return arm


def evaluateArm(arm: Arm) -> List[Pnt]:
    lengths = [link.llen for link, _ in arm.comp]
    pnts = [Pnt(0, 0)]
    for l in lengths:
        pnts.append(pnts[-1] + Pnt(0, l))
    pnts.append(pnts[-1] + Pnt(0, 0.1))
    return pnts


def dance(time: Second, arm: Arm) -> Control:
    return [-2 * math.sin(2.2 * time), -2 * math.cos(2.3 * time),  2 * math.sin(2.4 * time),  2 * math.cos(2.5 * time)
            ]

# Exercise 4


def handleCollision(snap1: Snapshot, snap2: Snapshot, time: Second) -> Tuple[Pnt, Vec]:
    return (Pnt(0, 0), Vec(0, 0))

# Exercise 5


def inverse(arm: Arm, seg: Seg) -> List[Radian]:
    return [0.0] * NUMLINKS

# Exercise 6


def plan(current_time: Second, arm: Arm, time_bound: Second,
         seg: Seg, velocity: Vec) -> Control:
    return [0.0] * NUMLINKS

# Exercise 7


def action(time: Second, item: Item, arm: Arm, ball: BallState) -> Control:
    return [-10 * math.sin(2.2 * time), -10 * math.cos(2.3 * time),  10 * math.sin(2.4 * time),  10 * math.cos(2.5 * time)]
