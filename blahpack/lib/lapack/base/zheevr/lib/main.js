'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zheevr = require( './zheevr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zheevr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zheevr;
