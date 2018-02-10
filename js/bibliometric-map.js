webpackJsonp([7],{

/***/ 24:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery__ = __webpack_require__(0);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_jquery__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__ = __webpack_require__(3);




__WEBPACK_IMPORTED_MODULE_0_jquery___default()(() => {
  const nodeData = {
    impact: {
      label: ['Impact factor'],
      type: 'bibliometric',
      x: w => -w / 8,
      y: h => h / 8
    },
    etc: {
      label: ['...'],
      type: 'problem',
      x: w => -w / 8 - 10,
      y: h => h / 8 + 10
    },
    review: {
      label: ['Favors review', 'articles'],
      type: 'problem',
      x: w => -w / 8 - 15,
      y: h => h / 8 + 15
    },
    'self-cite': {
      label: ['Pressure to', 'self-cite'],
      type: 'problem',
      x: w => -w / 8 - 20,
      y: h => h / 8 + 20
    },
    replication: {
      label: ['Biased against', 'replications'],
      type: 'problem',
      x: w => -w / 8 - 25,
      y: h => h / 8 + 25
    },
    entropy: {
      label: ['Entropic'],
      type: 'bibliometric',
      x: w => w / 8,
      y: h => -h / 8
    },
    single: {
      label: ['Single study'],
      type: 'example',
      x: w => w / 8 + 5,
      y: h => -h / 8 - 10
    },
    four: {
      label: ['Four studies'],
      type: 'example',
      x: w => w / 8 + 10,
      y: h => -h / 8 - 15
    },
    aggregates: {
      label: ['Aggregates sensibly'],
      type: 'benefit',
      x: w => w / 8 + 25,
      y: h => -h / 8 - 25
    },
    design: {
      label: ['Accounts for', 'study design'],
      type: 'benefit',
      x: w => w / 8 + 30,
      y: h => -h / 8 - 30
    },
    'repli-beni': {
      label: ['Handles replications'],
      type: 'benefit',
      x: w => w / 8 + 35,
      y: h => -h / 8 - 35
    },
    gradated: {
      label: ['Gradated citations'],
      type: 'benefit',
      x: w => w / 8 + 40,
      y: h => -h / 8 - 40
    },
    complicated: {
      label: ['Relatively complicated', 'to calculate'],
      type: 'problem',
      x: w => w / 8 + 45,
      y: h => -h / 8 - 45
    },
    dependence: {
      label: ['Relies on', 'degree of dependence'],
      type: 'problem',
      x: w => w / 8 + 50,
      y: h => -h / 8 - 50
    },
    incentive: {
      label: ['Not incentive-compatible'],
      type: 'problem',
      x: w => w / 8 + 55,
      y: h => -h / 8 - 55
    }
  };

  const linkData = [{ source: 'review', target: 'impact', type: 'describes' }, { source: 'self-cite', target: 'impact', type: 'describes' }, { source: 'replication', target: 'impact', type: 'describes' }, { source: 'etc', target: 'impact', type: 'describes' }, { source: 'single', target: 'entropy', type: 'illustrates' }, { source: 'four', target: 'entropy', type: 'illustrates' }, { source: 'aggregates', target: 'entropy', type: 'describes' }, { source: 'design', target: 'entropy', type: 'describes' }, { source: 'repli-beni', target: 'entropy', type: 'describes' }, { source: 'gradated', target: 'entropy', type: 'describes' }, { source: 'complicated', target: 'entropy', type: 'describes' }, { source: 'dependence', target: 'entropy', type: 'describes' }, { source: 'incentive', target: 'entropy', type: 'describes' }];

  const nodeTypeData = [{ type: 'problem', label: ['Problem'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].square }, { type: 'benefit', label: ['Benefit'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].diamond }, { type: 'bibliometric', label: ['Bibliometric'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].circle }, { type: 'example', label: ['Example'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].pentagon }];

  const linkTypeData = [{ type: 'illustrates', label: ['Illustrates'] }, { type: 'describes', label: ['Describes'] }];

  __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["a" /* handler */].apply(null, Object(__WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["b" /* mkMap */])('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData));
});

/***/ })

},[24]);
//# sourceMappingURL=bibliometric-map.js.map