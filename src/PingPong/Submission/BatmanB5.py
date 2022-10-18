# Alkiviadis Pavlou(2025930), Hui En Lin(8098735)
# PingPong Simulator v3.1.3
# Assignment B4
import logging
import numpy as np
from typing import Optional, Tuple
from PPData import * 
from itertools import accumulate

# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
SPEED_CAP = 2.0
ACCEL_CAP = 5.0
LOGGING_LEVEL = logging.ERROR
EPS = 0.0001

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

#Exercise 2, best practice provided by lecturers
def detectCollision(snap1: Snapshot, snap2: Snapshot) -> Optional[Second]:
    pocos = potentialCollisions(snap1, snap2)
    time = None
    for poco in pocos:
        if validPoco(poco):
            return collisionTime(snap1, snap2, poco[0])

    return time

def almostZero(number: float) -> bool:
    return abs(number) < EPS

def betweenZeroAndOne(number: float) -> bool:
    return number >= 0 and number <= 1

def validPoco(poco: List[float]) -> bool:
    return betweenZeroAndOne(poco[0]) and betweenZeroAndOne(poco[1])

def collisionTime(snap1: Snapshot, snap2: Snapshot, t: float) -> Second:
    return (1 - t) * snap1.time + t * snap2.time

def computeSegParameter(xa: float, xb: float, xc: float, xd: float, ya: float, yb: float, yc:float, yd: float, t: float) -> Optional[float]:
    f = 0.0
    x_zero = almostZero(xa + xc * t)
    y_zero = almostZero(ya + yc * t)
    if x_zero and y_zero:
        f = None
    elif x_zero:
        f = (yd - yb * t) / (ya + yc * t)
    elif y_zero:
        f = (xd - xb * t) / (xa + xc * t)
    else:
        f1 = (xd - xb * t) / (xa + xc * t)
        f2 = (yd - yb * t) / (ya + yc * t)
        if betweenZeroAndOne(f1):
            f = f1
        else:
            f = f2

    return f

def potentialCollisions(snap1: Snapshot, snap2: Snapshot) -> List[List[float]]:
    xb0, yb0 = snap1.ball.x, snap1.ball.y
    xb1, yb1 = snap2.ball.x, snap2.ball.y
    
    c0, d0 = snap1.segment.p, snap1.segment.q
    xc0, yc0 = c0.x, c0.y
    xd0, yd0 = d0.x, d0.y

    c1, d1 = snap2.segment.p, snap2.segment.q
    xc1, yc1 = c1.x, c1.y
    xd1, yd1 = d1.x, d1.y

    xa = xd0 - xc0
    ya = yd0 - yc0
    xb = xb0 - xc0 + xc1 - xb1
    yb = yb0 - yc0 + yc1 - yb1 
    xc = xc0 - xd0 + xd1 - xc1
    yc = yc0 - yd0 + yd1 - yc1
    xd = xb0 - xc0
    yd = yb0 - yc0

    i = xd * ya - yd * xa
    j = xd * yc - xb * ya - yd * xc + yb * xa
    k = yb * xc - xb * yc

    ts = solveQuadraticEq(k, j, i)
    fs = []
    for t in ts:
        f = computeSegParameter(xa, xb, xc, xd, ya, yb, yc, yd, t)
        if f != None:
            fs.append(f) 

    return sorted(zip(ts, fs))

def solveAZero(a: float, b: float, c: float) -> List[float]:
    return [-c / b]

def solveBZero(a: float, b: float, c: float) -> List[float]:
    val = -c / a
    val_sign = math.copysign(1.0, val)
    if val_sign < 0:
        return []
    else:
        return [math.sqrt(val)]

def solveCZero(a: float, b: float, c: float) -> List[float]:
    return sorted([0, -b / a])

def solveNonZero(a: float, b: float, c: float) -> List[float]:
    d = (b * b) - 4 * a * c

    if almostZero(d):
        return [-b / (2 * a)]
    elif d > 0:
        return [((-b) - math.sqrt(d)) / (2 * a), ((-b) + math.sqrt(d)) / (2 * a)]
    else:
        return []

def solveQuadraticEq(a: float, b: float, c: float) -> List[float]:
    res = []
    a_zero = almostZero(a)
    b_zero = almostZero(b)
    c_zero = almostZero(c)
    
    if c_zero and (a_zero or b_zero):
        res = [0] 
    elif a_zero and b_zero:
        res = []
    elif c_zero:
        res = solveCZero(a, b, c) 
    elif b_zero:
        res = solveBZero(a, b, c)
    elif a_zero:
        res = solveAZero(a, b, c)
    elif almostZero(a / b) or almostZero(a / c):
        res = solveQuadraticEq(0.0, b, c)
    else:
        res = solveNonZero(a, b, c)

    return res
        
# Exercise 3
EPS = 0.00000000001

def almostZero(number: float) -> bool:
    if abs(number) < EPS:
        return True
    return False

def cap(real: float, capval: float) -> float:
    realabs = abs(real)
    if almostZero(realabs - capval) or realabs > capval:
        if real < 0:
            return -capval
        return capval

    return real

def capspeed(speed: float) -> float:
    return cap(speed, SPEED_CAP)

def capaccel(accel: float) -> float:
    return cap(accel, ACCEL_CAP)

def controlArm(time: Second, control: Control, arm: Arm) -> Arm:
    # Go through all joints and apply the acceleration
    applyControl(time, control, arm)
    
    # Move the angle for each joint accordingly
    advanceArm(time, arm)

    return arm

def applyControl(time: Second, control: Control, arm: Arm) -> Arm:
    for (_, joint), accel in zip(arm.comp, control.accelerations):
        controlJoint(time, accel, joint)

def controlJoint(time: Second, accel: RadianPerSquareSecond, joint: Joint):
    joint.jvel = capspeed(joint.jvel + capaccel(accel) * time)

def advanceArm(time: Second, arm: Arm):
    for _, joint in arm.comp:
        advanceJoint(time, joint)

def advanceJoint(time: Second, joint: Joint):
    joint.jang = joint.jang + joint.jvel * time

def evaluateArm(arm: Arm) -> List[Pnt]:
    # Create transformation matrices
    ts = makeGlobal(transformations(arm))

    # Find every point starting from (0,0) (as (0,0,1) in homogenous coordinates)
    vs = [t @ np.array([[0.0],[0.0],[1.0]]) for t in ts]

    # Remove doubled entries via dict.fromkeys and store as tuples of floats, also implicitly transforms from homogenous coordinates to 2D
    single_pnts = list(dict.fromkeys(map(lambda v: (v[0][0], v[1][0]), vs)))

    # Make points from the tuples
    pnts = [Pnt(p[0], p[1]) for p in single_pnts]

    return pnts

def makeGlobal(ts: list(np.array)):
    gts = list(accumulate(ts, np.matmul, initial=identity()))
    return gts

def transformations(arm: Arm) -> list(np.array):
    ts = []
    for link, joint in arm.comp:
        tlink, rjoint = transformation(link, joint)
        ts.append(tlink)
        ts.append(rjoint)

    ts.append(transLink(arm.bat))

    return ts

def transformation(link: Link, joint: Joint) -> Tuple[np.array, np.array]:
    return transLink(link), rotJoint(joint)

def transLink(link: Link) -> np.array:
    return translation(Vec(0, link.llen))

def rotJoint(joint: Joint) -> np.array:
    return rotation(joint.jang)

def dance(time: Second, arm: Arm) -> Control:
    return [   20 * math.sin (6.0 * time)
           ,  -20 * math.cos (6.0 * time)
           ,   20 * math.sin (6.0 * time)
           ,  -20 * math.cos (6.0 * time)
           ]

# Exercise 4
def handleCollision(snap1: Snapshot, snap2: Snapshot, time: Second) -> Tuple[Pnt, Vec]:
    # convert to numpy array for better calculation
    b1, b2 = PntToNparray(snap1.ball), PntToNparray(snap2.ball)
    p1, p2 = PntToNparray(snap1.segment.p), PntToNparray(snap2.segment.p)
    q1, q2 = PntToNparray(snap1.segment.q), PntToNparray(snap2.segment.q)
    t1, t2 = snap1.time, snap2.time
    t = time - t1
    
    # velocity of the ball
    vb = (b2 - b1) / (t2 - t1)

    # position of the collision
    c = b1 + (vb * t)

    # velocity of the collision point
    vp = (p2 - p1) / (t2 - t1)
    vq = (q2 - q1) / (t2 - t1)
    p = p1 + (vp * t)
    q = q1 + (vq * t)
    pqNorm = np.linalg.norm(q - p)
    pcNorm = np.linalg.norm(c - p)
    pcWeight = pcNorm / pqNorm
    vc = (vp * (1 - pcWeight)) + (vq * (pcWeight))

    # calculate the relative velocity, reverse the result to make it point toward normal vector
    rv = -(vb - vc)

    # calculate the normal vector for reflect the relative velocity
    pq = (q - p) / np.linalg.norm((q - p)) # normalize the vector

    # reflect rv about the segement, r = v - (2 * (v ‧ N) * N), which N is the normalized normal vector
    reflection = rv - (2 * np.dot(rv, pq) * pq)

    # add the velocity of segment with the reflection vector
    v = (reflection + vc)

    return (Pnt(c[0], c[1]), Vec(v[0], v[1]))

# convert to numpy array for calculation
def PntToNparray(p: Pnt):
    return np.array([p.x, p.y, 0])

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
