
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztgex2 = require( './ztgex2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgex2, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgex2;
