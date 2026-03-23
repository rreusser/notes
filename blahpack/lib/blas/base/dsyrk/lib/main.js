

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsyrk = require( './dsyrk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyrk, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyrk;
