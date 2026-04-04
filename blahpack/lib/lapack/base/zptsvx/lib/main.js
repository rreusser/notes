
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zptsvx = require( './zptsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zptsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zptsvx;
