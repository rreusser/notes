
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgeevx = require( './zgeevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeevx;
