'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zposvx = require( './zposvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zposvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zposvx;
