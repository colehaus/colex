webpackJsonp([3],{

/***/ 19:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery__ = __webpack_require__(0);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_jquery__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__ = __webpack_require__(3);




__WEBPACK_IMPORTED_MODULE_0_jquery___default()(() => {
  const nodeData = {
    permissive: {
      label: ['Too permissive'],
      type: 'problem'
    },
    restrictive: {
      label: ['Too restrictive'],
      type: 'problem'
    },
    'quorum-def': {
      label: ['Traditional quorum'],
      type: 'technique'
    },
    'freq-bounds': {
      label: ['Credible bounds'],
      type: 'technique'
    },
    bayes: {
      label: ['Confidence bounds'],
      type: 'technique'
    },
    random: {
      label: ['Assumed randomness'],
      type: 'problem'
    },
    'post-hoc': {
      label: ['Only post-hoc', 'declaration', 'of quorum'],
      type: 'problem'
    },
    complicated: {
      label: ['More complicated'],
      type: 'problem'
    }
  };

  const linkData = [{ source: 'permissive', target: 'quorum-def', type: 'describes' }, { source: 'restrictive', target: 'quorum-def', type: 'describes' }, { source: 'random', target: 'quorum-def', type: 'describes' }, { source: 'random', target: 'freq-bounds', type: 'describes' }, { source: 'random', target: 'bayes', type: 'describes' }, { source: 'post-hoc', target: 'freq-bounds', type: 'describes' }, { source: 'post-hoc', target: 'bayes', type: 'describes' }, { source: 'complicated', target: 'freq-bounds', type: 'describes' }, { source: 'complicated', target: 'bayes', type: 'describes' }];

  const nodeTypeData = [{ type: 'technique', label: ['Technique'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].circle }, { type: 'problem', label: ['Problem'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].square }];

  const linkTypeData = [{ type: 'describes', label: ['Describes'] }];

  __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["a" /* handler */].apply(null, Object(__WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["b" /* mkMap */])('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData));
});

/***/ })

},[19]);
//# sourceMappingURL=quorum-map.js.map