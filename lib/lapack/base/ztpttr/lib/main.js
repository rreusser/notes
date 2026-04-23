'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztpttr = require( './ztpttr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztpttr, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztpttr;
