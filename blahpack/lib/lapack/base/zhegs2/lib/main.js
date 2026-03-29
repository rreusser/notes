'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhegs2 = require( './zhegs2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhegs2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhegs2;
