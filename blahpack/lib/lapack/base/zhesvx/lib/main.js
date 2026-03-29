'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhesvx = require( './zhesvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhesvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhesvx;
