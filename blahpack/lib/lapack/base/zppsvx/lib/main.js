
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zppsvx = require( './zppsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zppsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zppsvx;
