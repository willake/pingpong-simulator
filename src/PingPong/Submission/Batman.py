# Alkiviadis Pavlou(2025930), Hui En Lin(8098735)
# PingPong Simulator v3.1.2
# Assignment B2
import logging
import numpy as np
from typing import Optional, Tuple
from PPData import *

# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
LOGGING_LEVEL = logging.ERROR

NUMLINKS = 4

BAT_COLOR = "#242424"

LINK_COLORS = ["#242424", "#7f8086", "#7f8086", "#7f8086"]
LINK_LENGTHS = [0.3] * NUMLINKS

JOINT_COLORS = ["#fdff00", "#505c7c", "#242424", "#fdff00"]
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
    # b1 means ball in snap1, b2 means ball in snap2
    # a = b1, b = b2 - b1
    # c = p1, d = p2 - p1
    # e = q1, f = q2 - q1
    #
    # In here, B means ball as Pnt, P means p of segement, Q means q of segment 
    # if B lies on the linear function of segment then
    # the cross product of BP and BQ should be 0
    # BP x BQ = 0
    #
    # b1 means ball in snap1, b2 means ball in snap2
    # t is a rate between 0 and 1 in total time (t2 - t1)
    # Bx = b1x + ((b2x - b1x) * t)
    # By = b1y + ((b2y - b1y) * t)
    # ...
    # In the following code we replace b1 as a and (b2 - b1) as b, and so on
    # then we calculate BP and BQ
    # BPx = (cx + (dx * t)) - (ax + (bx * t))
    # BPy = (cy + (dy * t)) - (ay + (by * t))
    # BQx = (ex + (fx * t)) - (ax + (bx * t))
    # BQy = (ey + (fy * t)) - (ay + (by * t))
    # then do cross product as
    # (BPx * BQy) - (BPy * BQx) = 0
    # 
    # finally we got a quadratic equation
    # A * t^2 + B * t + C = 0
    # A = (dx*fy) - (bx*fy) - (dx*by) - (dy*fx) + (by*fx) + (dy*bx)
    # B = (dx*ey) + (cx*fy) - (bx*ey) - (ax*fy) - (dx*ay) - (cx*by)
    # - (dy*ex) - (cy*fx) + (by*ex) + (ay*fx) + (dy*ax) + (cy*bx)
    # C = (cx*ey) - (ax*ey) - (cx*ay) - (cy*ex) + (ay*ex) + (cy*ax)
    # and we could get two answer of t, and t should between 0 and 1
    # since we have a probably answer, we could assert whether B is between PQ
    # the equation is distance(BP) + distance(BQ) = distance(PQ)

    ax = snap1.ball.x
    ay = snap1.ball.y
    bx = snap2.ball.x - snap1.ball.x
    by = snap2.ball.y - snap1.ball.y
    cx = snap1.segment.p.x
    cy = snap1.segment.p.y
    dx = snap2.segment.p.x - snap1.segment.p.x
    dy = snap2.segment.p.y - snap1.segment.p.y
    ex = snap1.segment.q.x
    ey = snap1.segment.q.y
    fx = snap2.segment.q.x - snap1.segment.q.x
    fy = snap2.segment.q.y - snap1.segment.q.y

    A = dx*fy-bx*fy-dx*by-dy*fx+by*fx+dy*bx
    B = dx*ey+cx*fy-bx*ey-ax*fy-dx*ay-cx*by-dy*ex-cy*fx+by*ex+ay*fx+dy*ax+cy*bx
    C = cx*ey-ax*ey-cx*ay-cy*ex+ay*ex+cy*ax

    # normally t = -B +- sqrt(B**2 - 4 * A * C) / 2A
    # if A = 0 then t = -B/C
    # if A = 0 and B = 0 then there is no answer
    if math.isclose(A, 0) :
        if math.isclose(B, 0):
            return None
        else:
            t = -C/B
            if not t < 0 and not t > 1:

                # check dist(BP) + dist(BQ) = dist(PQ)
                Ball = snap1.ball + ((snap2.ball - snap1.ball) * t)
                P = snap1.segment.p + ((snap2.segment.p - snap1.segment.p) * t)
                Q = snap1.segment.q + ((snap2.segment.q - snap1.segment.q) * t)

                VB = np.array((Ball.x, Ball.y))
                VP = np.array([P.x, P.y])
                VQ = np.array([Q.x, Q.y])

                BP = np.linalg.norm(VP - VB)
                BQ = np.linalg.norm(VQ - VB)
                PQ = np.linalg.norm(VQ - VP)

                if math.isclose((BP + BQ) - PQ, 0) :
                    return Second((snap1.time+(snap2.time-snap1.time)*(t)))
                else :
                    return None
            else:
                return None

    d = (B*B)-(4*A*C)
    t = 0
    
    # return none when d is negative since a negative number can not do sqrt
    if d < 0:
        return None

    t1 = (-B+math.sqrt(d))/(2*A)
    t2 = (-B-math.sqrt(d))/(2*A)

    # one of t should between 0 and 1
    if not t1 < 0 and not t1 > 1 :
        t = t1
    elif not t2 < 0 and not t2 > 1 :
        t = t2
    else :
        return None

    # check dist(BP) + dist(BQ) = dist(PQ)
    Ball = snap1.ball + ((snap2.ball - snap1.ball) * t)
    P = snap1.segment.p + ((snap2.segment.p - snap1.segment.p) * t)
    Q = snap1.segment.q + ((snap2.segment.q - snap1.segment.q) * t)

    VB = np.array((Ball.x, Ball.y))
    VP = np.array([P.x, P.y])
    VQ = np.array([Q.x, Q.y])

    BP = np.linalg.norm(VP - VB)
    BQ = np.linalg.norm(VQ - VB)
    PQ = np.linalg.norm(VQ - VP)
    
    if math.isclose((BP + BQ) - PQ, 0) :
        return Second((snap1.time+(snap2.time-snap1.time)*(t)))
    else :
        return None
        
# Exercise 3
def controlArm(time: Second, control: Control, arm: Arm) -> Arm:
    return arm

def evaluateArm(arm: Arm) -> List[Pnt]:
    lengths = [link.llen for link, _ in arm.comp]
    angles = [joint.jang for _, joint in arm.comp]
    angles.insert(0, 90)
    pnts = [Pnt(0, 0)]
    index = 0
    angle = 0

    # x = l1 c(θ1) + l2 c(θ1 + θ2) + l3 c(θ1 + θ2 + θ3)...
    # y = l1 s(θ1) + l2 s(θ1 + θ2) + l3 s(θ1 + θ2 + θ3)...
    for l in lengths:
        pnt = Pnt(pnts[index].x, pnts[index].y)
        angle += angles[index]
        pnt.x += l * math.cos(angle)
        pnt.y += l * math.sin(angle)
        pnts.append(pnt)
        index += 1

    # bat
    pnt = Pnt(pnts[index].x, pnts[index].y)
    angle += angles[index]
    pnt.x += arm.bat.llen * math.cos(angle)
    pnt.y += arm.bat.llen * math.sin(angle)
    pnts.append(pnt)

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
