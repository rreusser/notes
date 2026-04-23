'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zheevx = require( './zheevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zheevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zheevx;
