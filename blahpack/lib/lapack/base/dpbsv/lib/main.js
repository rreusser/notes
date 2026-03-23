

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpbsv = require( './dpbsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbsv;
