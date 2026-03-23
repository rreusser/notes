

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlabrd = require( './zlabrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlabrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlabrd;
