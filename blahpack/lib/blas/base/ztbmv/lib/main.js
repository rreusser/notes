'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztbmv = require( './ztbmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztbmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztbmv;
