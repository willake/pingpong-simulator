# Alkiviadis Pavlou(2025930), Hui En Lin(8098735)
# PingPong Simulator v3.1.3
# Assignment B4
import logging
import numpy as np
from typing import Optional, Tuple
from PPData import * 

# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
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
def controlArm(time: Second, control: Control, arm: Arm) -> Arm:
    length = len(arm.comp)
    joints = [joint for _ , joint in arm.comp]
    for index in range(length):
        
        acceleration = float(control.accelerations[index])

        if acceleration > 5.0:
                acceleration = 5.0
        v = joints[index].jvel + (acceleration * time)
        
        if v > 2.0:
            v = 2.0
        
        joints[index].jvel = v
        joints[index].jang = joints[index].jang + (v * time)
        
    return arm

def evaluateArm(arm: Arm) -> List[Pnt]:
    lengths = [link.llen for link, _ in arm.comp]
    angles = [joint.jang for _, joint in arm.comp]
    angles.insert(0, math.pi / 2)
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
