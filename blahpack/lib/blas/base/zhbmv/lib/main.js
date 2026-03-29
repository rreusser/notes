'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhbmv = require( './zhbmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhbmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhbmv;
