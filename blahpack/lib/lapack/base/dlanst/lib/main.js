

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlanst = require( './dlanst.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlanst, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlanst;
