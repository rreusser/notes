

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var ztbsv = require( './ztbsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztbsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztbsv;
