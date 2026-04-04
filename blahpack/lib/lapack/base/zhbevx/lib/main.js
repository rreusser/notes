
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhbevx = require( './zhbevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhbevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhbevx;
