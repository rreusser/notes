

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtbsv = require( './dtbsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtbsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtbsv;
