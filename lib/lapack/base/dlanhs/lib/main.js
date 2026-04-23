
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlanhs = require( './dlanhs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlanhs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlanhs;
