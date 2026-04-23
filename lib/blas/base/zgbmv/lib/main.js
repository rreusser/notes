'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgbmv = require( './zgbmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbmv;
