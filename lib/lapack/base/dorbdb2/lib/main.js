
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorbdb2 = require( './dorbdb2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorbdb2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorbdb2;
