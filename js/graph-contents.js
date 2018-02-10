webpackJsonp([4],{

/***/ 23:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery__ = __webpack_require__(0);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_jquery__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__ = __webpack_require__(3);




__WEBPACK_IMPORTED_MODULE_0_jquery___default()(() => {
  const nodeData = {
    major: {
      label: ['All men are mortal'],
      type: 'major'
    },
    minor: {
      label: ['Socrates is a man'],
      type: 'minor'
    },
    conclusion: {
      label: ['Socrates is mortal'],
      type: 'conclusion'
    }
  };

  const linkData = [{ source: 'major', target: 'conclusion', type: 'imply' }, { source: 'minor', target: 'conclusion', type: 'imply' }];

  const nodeTypeData = [{ type: 'major', label: ['Major premise'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].square }, { type: 'minor', label: ['Minor premise'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].diamond }, { type: 'conclusion', label: ['Conclusion'], shape: __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["c" /* shapes */].circle }];

  const linkTypeData = [{ type: 'imply', label: ['Jointly implies'] }];

  __WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["a" /* handler */].apply(null, Object(__WEBPACK_IMPORTED_MODULE_1_libs_arg_map__["b" /* mkMap */])('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData));
});

/***/ })

},[23]);
//# sourceMappingURL=graph-contents.js.map