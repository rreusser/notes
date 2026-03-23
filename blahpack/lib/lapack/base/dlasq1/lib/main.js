

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasq1 = require( './dlasq1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasq1, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasq1;
