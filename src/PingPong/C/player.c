#include "player.h"
#include <stdio.h>

char* callpython(char* message) {
    // printf("entering c\n");
    // printf("message\n");
    // printf(message);
    // printf("\n");

    // printf("init python\n");
    Py_Initialize();

    // printf("adding to path\n");
    PyObject *sys_path = PySys_GetObject("path");
    if(sys_path == NULL) {
        printf("Error C: Could not obtain sys-path.\n");
        return "";
    }

    PyList_Append(sys_path, PyUnicode_FromString("src/PingPong/Python"));
    
    // printf("getting module\n");
    PyObject* myModule = PyImport_ImportModule("CLink");
    if(myModule == NULL) {
        printf("Error C: Could not obtain 'CLink' module.\n");
        return "";
    }

    // printf("getting function\n");
    PyObject* myFunction = PyObject_GetAttrString(myModule, "handle_message");
    if(myFunction == NULL) {
        printf("Error C: Could not obtain 'handle_message' function.\n");
        return "";
    }

    // printf("preparing arguments\n");
    PyObject* myArgs = PyTuple_Pack(1, PyUnicode_FromString(message));
    if(myArgs == NULL) {
        printf("Error C: Could not create arguments.\n");
        return "";
    }

    // printf("calling function\n");
    PyObject* repl = PyObject_CallObject(myFunction, myArgs);
    if(repl == NULL) {
        printf("Error C: Reply is NULL.\n");
        return "";
    }

    // printf("extracting answer\n");
    Py_ssize_t size;
    PyObject* mybytes = PyUnicode_AsASCIIString(repl);
    if(mybytes == NULL) {
        printf("Error C: Could not convert response from python-unicode to python-bytes.\n");
        return "";
    }

	// printf("extracted bytes\n");
	char* myrepl = PyBytes_AsString(mybytes);
    if(myrepl == NULL) {
        printf("Error C: Could not convert response from python-bytes to c-string.\n");
        return "";
    }
    // printf(myrepl);
    // printf("\n");

    // printf("deconstructing\n");

    Py_DecRef(repl);
    Py_DecRef(myArgs);
    Py_DecRef(myFunction);
    Py_DecRef(myModule);


    // printf("returning\n");


    return myrepl;
}
