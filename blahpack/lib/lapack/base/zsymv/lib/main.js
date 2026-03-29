'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsymv = require( './zsymv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsymv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsymv;
