from PPJson import *
from PPData import Arm
from dataclasses import dataclass
from types import ModuleType
from importlib import import_module
import logging
import json
import string
import sys
import os
sys.path.append(os.path.realpath("src/PingPong/Python"))
sys.path.append(os.path.realpath("src/PingPong/Submission"))

################
# YOUR PLAYER MODULE
import Batman as eps
#
################


SETUP = "setup"
COL_DETECTION = "collision_detection"
COL_HANDLING = "collision_handling"
CONTROL_ARM = "control_arm"
EVALUATE_ARM = "evaluate_arm"
DANCE = "dance"
INVERSE = "inverse"
PLAN = "plan"
ACTION = "action"

NO_COLLISION = "no_collision"


def handle_message(message):
    logging.basicConfig(
        format="Python %(levelname)s: %(module)s:%(lineno)s %(message)s", level=eps.LOGGING_LEVEL)
    message = json.loads(message)
    m_type = message["type"]
    handler = None
    if m_type == SETUP:
        logging.debug(f"Received {SETUP} message")
        handler = handle_setup
    elif m_type == COL_DETECTION:
        logging.debug(f"Received {COL_DETECTION} message")
        handler = handle_collision_detection
    elif m_type == COL_HANDLING:
        logging.debug(f"Received {COL_HANDLING} message")
        handler = handle_collision_handling
    elif m_type == CONTROL_ARM:
        logging.debug(f"Received {CONTROL_ARM} message")
        handler = handle_control_arm
    elif m_type == EVALUATE_ARM:
        logging.debug(f"Received {EVALUATE_ARM} message")
        handler = handle_evaluate_arm
    elif m_type == DANCE:
        logging.debug(f"Received {DANCE} message")
        handler = handle_dance
    elif m_type == INVERSE:
        logging.debug(f"Received {INVERSE} message")
        handler = handle_inverse
    elif m_type == PLAN:
        logging.debug(f"Received {PLAN} message")
        handler = handle_plan
    elif m_type == ACTION:
        logging.debug(f"Received {ACTION} message")
        handler = handle_action

    if handler != None:
        m = handler(message)
        logging.info(f"returning {m}")
        return m
    else:
        logging.error(f"Received unknown {m_type} message")

    return None


def handle_setup(message):
    name = eps.name()
    arm = eps.make_arm()

    logging.debug(f"Sending arm {arm} and name {name}")

    return json.dumps(
        {
            "type": "setup",
            "name": name,
            "arm": arm
        },
        cls=PingPongJSONEncoder
    )


def handle_collision_detection(message):
    snap1, snap2 = parse_detect_collision(message)
    collision = eps.detectCollision(snap1, snap2)

    ret = {"type": "collision_detection"}
    if collision != None:
        logging.info(f"Collision detected: {collision}")
        ret["collision"] = collision
    else:
        logging.debug(f"No collision detected")
        ret["collision"] = NO_COLLISION

    return json.dumps(ret, cls=PingPongJSONEncoder)


def handle_collision_handling(message):
    snap1, snap2, time = parse_handle_collision(message)
    ball, velocity = eps.handleCollision(snap1, snap2, time)
    logging.info(f"Collision at {ball} new velocity {velocity}")

    return json.dumps(
        {
            "type": "collision_handling",
            "ball": ball,
            "velocity": velocity
        }, cls=PingPongJSONEncoder)


def handle_control_arm(message):
    time, control, thearm = parse_control_arm(message)
    new_arm = eps.controlArm(time, control, thearm)
    logging.info(f"New arm {new_arm}")

    return json.dumps(
        {
            "type": "control_arm",
            "arm": new_arm
        }, cls=PingPongJSONEncoder)


def handle_evaluate_arm(message):
    thearm = parse_evaluate_arm(message)
    locations = eps.evaluateArm(thearm)
    logging.info(f"Evaluated arm, result: {locations}")

    return json.dumps(
        {
            "type": "evaluate_arm",
            "locations": locations
        }, cls=PingPongJSONEncoder)


def handle_dance(message):
    time, thearm = parse_dance(message)
    control = eps.dance(time, thearm)
    logging.info(f"Dance control {control}")

    return json.dumps(
        {
            "type": "dance",
            "control": control
        })


def handle_inverse(message):
    thearm, seg = parse_inverse(message)
    angles = eps.inverse(thearm, seg)
    logging.info(f"Computed angles {angles}")

    return json.dumps(
        {
            "type": "inverse",
            "angles": angles
        })


def handle_plan(message):
    ctime, thearm, btime, seg, velocity = parse_plan(message)
    control = eps.plan(ctime, thearm, btime, seg, velocity)
    logging.info(f"Plan control {control}")

    return json.dumps(
        {
            "type": "plan",
            "control": control
        })


def handle_action(message):
    time, item, thearm, state = parse_action(message)
    control = eps.action(time, item, thearm, state)
    logging.info(f"New control vector {control}")

    return json.dumps(
        {
            "type": "action",
            "control": control
        }, cls=PingPongJSONEncoder)
