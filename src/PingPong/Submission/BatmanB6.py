# Alkiviadis Pavlou(2025930), Hui En Lin(8098735)
# PingPong Simulator v3.1.4
# Assignment B5
import logging
import numpy as np
from typing import Optional, Tuple
from PPData import * 
from itertools import accumulate, count
import copy

# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
SPEED_CAP = 2.0
ACCEL_CAP = 5.0
LOGGING_LEVEL = logging.ERROR
EPS = 0.00000000001

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

# Exercise 3
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

# Exercise 5
ITERATIONS = 1000
from itertools import cycle

def angles(arm: Arm) -> List[Radian]:
    return [j.jang for _, j in arm.comp]

def inverse(arm: Arm, seg: Seg) -> Optional[List[Radian]]:
    original = copy.deepcopy(arm)
    possible = solveInverse(ITERATIONS, arm, seg)

    if possible:
        return angles(arm)
    else:
        arm = original
        possible = solveInverse(ITERATIONS, arm, Seg(seg.q, seg.p))
        if possible:
            return angles(arm)
    
    return None

def solveInverse(iterations: int, arm: Arm, seg: Seg) -> bool:
    possible = ccd(iterations, seg.p, arm)
    setFinalAngle(arm, seg.q)

    return possible

def alength(arm: Arm):
    return len(arm.comp)

def ithjoint(i: int, arm: Arm) -> Joint:
    if i - 1 >= 0 and i - 1 < alength(arm):
        return arm.comp[i - 1][1]
    
    raise IndexError("Not enough joints.")

def addRadian(s: Radian, t: Radian) -> Radian:
    return (s + t) % (2 * math.pi)

def ccd(iterations: int, goal: Pnt, arm: Arm) -> bool:
    round = 0
    for i in cycle(range(1, alength(arm) + 1)):
        joint = ithjoint(i, arm)
        positions = evaluateArm(arm)
        pivot = positions[-2]

        distance = (goal - pivot).norm()
        if distance < EPS:
            break
        elif round == iterations:
            return False

        step(joint, positions[i], pivot, goal)

        round = round + 1
    
    return True
        
def step(joint: Joint, jpos: Pnt, pivot: Pnt, goal: Pnt):
    diff = angle((pivot - jpos), (goal - jpos))
    frac = 0.999 * diff
    joint.jang = addRadian(joint.jang, frac)

def setFinalAngle(arm: Arm, goal: Pnt):
    positions = evaluateArm(arm)
    fjoint = ithjoint(alength(arm), arm)
    tip = positions[-1]
    piv = positions[-2]
    u = tip - piv
    v = goal - piv
    dif = angle(u, v)

    fjoint.jang = addRadian(fjoint.jang, dif)

#Exercise 6
def plan(current_time: Second, arm: Arm, time_bound: Second, 
            seg: Seg, velocity: Vec) -> Control:
    return [0.0] * NUMLINKS