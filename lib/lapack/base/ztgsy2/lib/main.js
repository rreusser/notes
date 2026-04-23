
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztgsy2 = require( './ztgsy2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgsy2, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgsy2;
