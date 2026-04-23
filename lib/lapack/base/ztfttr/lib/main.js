'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztfttr = require( './ztfttr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztfttr, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztfttr;
